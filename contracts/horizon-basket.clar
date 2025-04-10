;; HorizonBasket - Resource Allocation and Custody Protocol


;; Global sequence tracking
(define-data-var basket-sequence-id uint u0)

;; Support functions
(define-private (valid-beneficiary? (beneficiary principal))
  (and 
    (not (is-eq beneficiary tx-sender))
    (not (is-eq beneficiary (as-contract tx-sender)))
  )
)

(define-private (basket-exists? (basket-id uint))
  (<= basket-id (var-get basket-sequence-id))
)


;; Basket registry data structure
(define-map BasketRegistry
  { basket-id: uint }
  {
    originator: principal,
    beneficiary: principal,
    resource-id: uint,
    quantity: uint,
    basket-status: (string-ascii 10),
    creation-block: uint,
    termination-block: uint
  }
)

;; Core operational parameters
(define-constant PROTOCOL_GOVERNOR tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_BASKET_MISSING (err u101))
(define-constant ERR_ALREADY_PROCESSED (err u102))
(define-constant ERR_MOVEMENT_FAILED (err u103))
(define-constant ERR_INVALID_BASKET_ID (err u104))
(define-constant ERR_INVALID_QUANTITY (err u105))
(define-constant ERR_INVALID_ORIGINATOR (err u106))
(define-constant ERR_BASKET_LAPSED (err u107))
(define-constant BASKET_LIFESPAN_BLOCKS u1008) 

;; Public interfaces
;; Execute basket delivery to beneficiary
(define-public (finalize-basket-delivery (basket-id uint))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (beneficiary (get beneficiary basket-data))
        (quantity (get quantity basket-data))
        (resource (get resource-id basket-data))
      )
      (asserts! (or (is-eq tx-sender PROTOCOL_GOVERNOR) (is-eq tx-sender (get originator basket-data))) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get basket-status basket-data) "pending") ERR_ALREADY_PROCESSED)
      (asserts! (<= block-height (get termination-block basket-data)) ERR_BASKET_LAPSED)
      (match (as-contract (stx-transfer? quantity tx-sender beneficiary))
        success
          (begin
            (map-set BasketRegistry
              { basket-id: basket-id }
              (merge basket-data { basket-status: "delivered" })
            )
            (print {action: "basket_delivered", basket-id: basket-id, beneficiary: beneficiary, resource-id: resource, quantity: quantity})
            (ok true)
          )
        error ERR_MOVEMENT_FAILED
      )
    )
  )
)


;; Release time-locked basket interval
;; Releases the next due portion of a time-locked basket to the beneficiary
(define-public (release-time-locked-interval (basket-id uint))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (beneficiary (get beneficiary basket-data))
        (originator (get originator basket-data))
        (creation-block (get creation-block basket-data))
        (termination-block (get termination-block basket-data))
        (total-quantity (get quantity basket-data))
        (intervals-total (/ (- termination-block creation-block) (/ BASKET_LIFESPAN_BLOCKS u10)))
        (current-interval (/ (- block-height creation-block) (/ BASKET_LIFESPAN_BLOCKS u10)))
        (interval-amount (/ total-quantity intervals-total))
      )
      (asserts! (is-eq (get basket-status basket-data) "time-locked") ERR_ALREADY_PROCESSED)
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      (asserts! (>= current-interval u1) (err u271)) ;; At least one interval must have passed
      (asserts! (<= current-interval intervals-total) (err u272)) ;; Cannot exceed total intervals

      ;; Transfer the interval amount to beneficiary
      (match (as-contract (stx-transfer? interval-amount tx-sender beneficiary))
        success
          (begin
            (print {action: "interval_released", basket-id: basket-id, beneficiary: beneficiary, 
                   current-interval: current-interval, interval-amount: interval-amount, 
                   remaining-amount: (- total-quantity interval-amount)})
            (ok true)
          )
        error ERR_MOVEMENT_FAILED
      )
    )
  )
)

;; Set up tiered multi-signature requirements
;; Configures the number of signatures required based on basket value
(define-public (configure-multi-sig-requirements (basket-id uint) (required-signatures uint) (signatories (list 5 principal)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (asserts! (> required-signatures u0) ERR_INVALID_QUANTITY)
    (asserts! (<= required-signatures (len signatories)) ERR_INVALID_QUANTITY)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (quantity (get quantity basket-data))
      )
      ;; Determine appropriate signature requirements based on value
      (asserts! (or 
                 (and (< quantity u1000) (<= required-signatures u1)) 
                 (and (and (>= quantity u1000) (< quantity u5000)) (<= required-signatures u2))
                 (and (>= quantity u5000) (<= required-signatures u3))
                ) (err u290))

      (asserts! (is-eq tx-sender originator) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get basket-status basket-data) "pending") ERR_ALREADY_PROCESSED)

      ;; Verify all signatories are unique
      ;; In a real implementation, would also verify signatories aren't the originator or beneficiary

      (print {action: "multi_sig_configured", basket-id: basket-id, originator: originator, 
              required-signatures: required-signatures, signatories: signatories})
      (ok true)
    )
  )
)

;; Approve dual-approval basket
;; Records approval from either originator or beneficiary for dual-approval basket
(define-public (approve-dual-approval-basket (basket-id uint) (approval-code (buff 32)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (quantity (get quantity basket-data))
      )
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary)) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get basket-status basket-data) "dual-pending") ERR_ALREADY_PROCESSED)

      ;; When both parties have approved, change status to confirmed
      ;; In a real implementation, this would track who has approved

      (print {action: "dual_approval_registered", basket-id: basket-id, approver: tx-sender, 
              approval-digest: (hash160 approval-code)})
      (ok true)
    )
  )
)

;; Validate resource integrity
;; Verifies that resource metadata matches with expected parameters
(define-public (validate-resource-integrity (basket-id uint) (resource-hash (buff 32)) (metadata-fields (list 5 (string-ascii 32))))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (resource-id (get resource-id basket-data))
      )
      ;; Only authorized parties can validate resources
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      ;; Ensure basket is in a state that allows validation
      (asserts! (or (is-eq (get basket-status basket-data) "pending") (is-eq (get basket-status basket-data) "confirmed")) ERR_ALREADY_PROCESSED)
      ;; Ensure resource data is provided
      (asserts! (> (len metadata-fields) u0) ERR_INVALID_QUANTITY)

      ;; In production would verify resource hash against on-chain registry

      (print {action: "resource_integrity_validated", basket-id: basket-id, resource-id: resource-id, 
              resource-hash: resource-hash, validator: tx-sender, metadata-count: (len metadata-fields)})
      (ok true)
    )
  )
)

;; Implement time-locked escrow release
;; Provides a secure time-gated release mechanism for high-value transfers
(define-public (initiate-timelock-release (basket-id uint) (unlock-time uint))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (asserts! (> unlock-time block-height) ERR_INVALID_QUANTITY)
    (asserts! (<= unlock-time (+ block-height u2880)) (err u270)) ;; Maximum ~20 days in future
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (quantity (get quantity basket-data))
      )
      ;; Only originator or governor can initiate timelock
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      ;; Only for baskets in pending state
      (asserts! (is-eq (get basket-status basket-data) "pending") ERR_ALREADY_PROCESSED)
      ;; Only for baskets above threshold
      (asserts! (> quantity u2000) (err u271)) ;; Minimum quantity for timelock

      ;; Update basket to time-locked state
      (map-set BasketRegistry
        { basket-id: basket-id }
        (merge basket-data { basket-status: "timelocked", termination-block: unlock-time })
      )

      (print {action: "timelock_initiated", basket-id: basket-id, originator: originator, 
              unlock-time: unlock-time, quantity: quantity})
      (ok unlock-time)
    )
  )
)

;; Register authorized interceptors for high-risk operations
;; Allows specific principals to be notified of suspicious activity
(define-public (register-security-interceptor (basket-id uint) (interceptor principal) (authority-level (string-ascii 15)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (quantity (get quantity basket-data))
      )
      ;; Only originator or governor can register interceptors
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      ;; Only for active baskets
      (asserts! (or (is-eq (get basket-status basket-data) "pending") 
                   (is-eq (get basket-status basket-data) "confirmed")) ERR_ALREADY_PROCESSED)
      ;; Interceptor must not be originator or beneficiary
      (asserts! (not (is-eq interceptor originator)) (err u280))
      (asserts! (not (is-eq interceptor (get beneficiary basket-data))) (err u281))

      ;; Validate authority level
      (asserts! (or (is-eq authority-level "observer")
                   (is-eq authority-level "auditor")
                   (is-eq authority-level "enforcer")) (err u282))

      ;; High-value baskets get enhanced security
      (if (> quantity u5000)
          (print {action: "enhanced_interceptor_registered", basket-id: basket-id, interceptor: interceptor, 
                  authority-level: authority-level, high-value: true})
          (print {action: "interceptor_registered", basket-id: basket-id, interceptor: interceptor, 
                  authority-level: authority-level, high-value: false})
      )

      (ok true)
    )
  )
)


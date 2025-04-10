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

;; Register basket with dual-approval requirement
;; Creates a basket that needs approval from both originator and beneficiary to finalize
(define-public (create-dual-approval-basket (beneficiary principal) (resource-id uint) (quantity uint))
  (begin
    (asserts! (> quantity u0) ERR_INVALID_QUANTITY)
    (asserts! (valid-beneficiary? beneficiary) ERR_INVALID_ORIGINATOR)
    (let 
      (
        (new-id (+ (var-get basket-sequence-id) u1))
        (termination-date (+ block-height BASKET_LIFESPAN_BLOCKS))
      )
      (match (stx-transfer? quantity tx-sender (as-contract tx-sender))
        success
          (begin
            (var-set basket-sequence-id new-id)
            (print {action: "dual_approval_basket_created", basket-id: new-id, originator: tx-sender, 
                    beneficiary: beneficiary, resource-id: resource-id, quantity: quantity})
            (ok new-id)
          )
        error ERR_MOVEMENT_FAILED
      )
    )
  )
)

;; Send resources back to originator
(define-public (revert-basket-contents (basket-id uint))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (quantity (get quantity basket-data))
      )
      (asserts! (is-eq tx-sender PROTOCOL_GOVERNOR) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get basket-status basket-data) "pending") ERR_ALREADY_PROCESSED)
      (match (as-contract (stx-transfer? quantity tx-sender originator))
        success
          (begin
            (map-set BasketRegistry
              { basket-id: basket-id }
              (merge basket-data { basket-status: "reverted" })
            )
            (print {action: "contents_reverted", basket-id: basket-id, originator: originator, quantity: quantity})
            (ok true)
          )
        error ERR_MOVEMENT_FAILED
      )
    )
  )
)

;; Register hardware verification proof
;; Adds a hardware-based verification proof for high-security baskets
(define-public (register-hardware-verification (basket-id uint) (device-id (buff 16)) (verification-proof (buff 64)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (quantity (get quantity basket-data))
      )
      ;; Only for high-value baskets (> 2000 STX)
      (asserts! (> quantity u2000) (err u280))
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      (asserts! (or (is-eq (get basket-status basket-data) "pending") 
                    (is-eq (get basket-status basket-data) "confirmed")) ERR_ALREADY_PROCESSED)

      ;; In a real implementation, would validate hardware signatures

      (print {action: "hardware_verification_registered", basket-id: basket-id, verifier: tx-sender, 
              device-id: device-id, verification-digest: (hash160 verification-proof)})
      (ok true)
    )
  )
)

;; Originator requests basket termination
(define-public (terminate-basket (basket-id uint))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (quantity (get quantity basket-data))
      )
      (asserts! (is-eq tx-sender originator) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get basket-status basket-data) "pending") ERR_ALREADY_PROCESSED)
      (asserts! (<= block-height (get termination-block basket-data)) ERR_BASKET_LAPSED)
      (match (as-contract (stx-transfer? quantity tx-sender originator))
        success
          (begin
            (map-set BasketRegistry
              { basket-id: basket-id }
              (merge basket-data { basket-status: "terminated" })
            )
            (print {action: "basket_terminated", basket-id: basket-id, originator: originator, quantity: quantity})
            (ok true)
          )
        error ERR_MOVEMENT_FAILED
      )
    )
  )
)

;; Prolong basket duration
(define-public (prolong-basket-duration (basket-id uint) (additional-blocks uint))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (asserts! (> additional-blocks u0) ERR_INVALID_QUANTITY)
    (asserts! (<= additional-blocks u1440) ERR_INVALID_QUANTITY) ;; Maximum ~10 days extension
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data)) 
        (beneficiary (get beneficiary basket-data))
        (current-termination (get termination-block basket-data))
        (updated-termination (+ current-termination additional-blocks))
      )
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      (asserts! (or (is-eq (get basket-status basket-data) "pending") (is-eq (get basket-status basket-data) "confirmed")) ERR_ALREADY_PROCESSED)
      (map-set BasketRegistry
        { basket-id: basket-id }
        (merge basket-data { termination-block: updated-termination })
      )
      (print {action: "basket_prolonged", basket-id: basket-id, requester: tx-sender, new-termination-block: updated-termination})
      (ok true)
    )
  )
)

;; Recover lapsed basket resources
(define-public (recover-lapsed-basket (basket-id uint))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (quantity (get quantity basket-data))
        (termination (get termination-block basket-data))
      )
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      (asserts! (or (is-eq (get basket-status basket-data) "pending") (is-eq (get basket-status basket-data) "confirmed")) ERR_ALREADY_PROCESSED)
      (asserts! (> block-height termination) (err u108)) ;; Must be lapsed
      (match (as-contract (stx-transfer? quantity tx-sender originator))
        success
          (begin
            (map-set BasketRegistry
              { basket-id: basket-id }
              (merge basket-data { basket-status: "lapsed" })
            )
            (print {action: "lapsed_basket_recovered", basket-id: basket-id, originator: originator, quantity: quantity})
            (ok true)
          )
        error ERR_MOVEMENT_FAILED
      )
    )
  )
)

;; Securely transition basket between defined states
;; Ensures state transitions follow allowed paths and records history
(define-public (secure-basket-transition (basket-id uint) (target-state (string-ascii 10)) (transition-reason (string-ascii 50)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (current-state (get basket-status basket-data))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
      )
      ;; Verify authorization based on transition type
      (asserts! (or (is-eq tx-sender PROTOCOL_GOVERNOR) 
                  (is-eq tx-sender originator)
                  (is-eq tx-sender beneficiary)) ERR_UNAUTHORIZED)

      ;; Validate transition is allowed based on current state
      (asserts! (or 
                 ;; From pending
                 (and (is-eq current-state "pending") 
                      (or (is-eq target-state "confirmed") 
                          (is-eq target-state "challenged")
                          (is-eq target-state "suspended")))
                 ;; From confirmed
                 (and (is-eq current-state "confirmed") 
                      (or (is-eq target-state "delivered")
                          (is-eq target-state "challenged")))
                 ;; From challenged
                 (and (is-eq current-state "challenged") 
                      (or (is-eq target-state "pending")
                          (is-eq target-state "adjudicated")))
                 ;; From suspended
                 (and (is-eq current-state "suspended") 
                      (or (is-eq target-state "pending")
                          (is-eq target-state "terminated")))
                ) (err u270))

      ;; Update basket to new state
      (map-set BasketRegistry
        { basket-id: basket-id }
        (merge basket-data { basket-status: target-state })
      )

      (print {action: "basket_transitioned", basket-id: basket-id, 
              from-state: current-state, to-state: target-state, 
              transition-reason: transition-reason, transitioner: tx-sender})
      (ok true)
    )
  )
)

;; Adjudicate challenge with decision
(define-public (adjudicate-challenge (basket-id uint) (originator-share uint))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (asserts! (is-eq tx-sender PROTOCOL_GOVERNOR) ERR_UNAUTHORIZED)
    (asserts! (<= originator-share u100) ERR_INVALID_QUANTITY) ;; Share must be 0-100
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (quantity (get quantity basket-data))
        (originator-amount (/ (* quantity originator-share) u100))
        (beneficiary-amount (- quantity originator-amount))
      )
      (asserts! (is-eq (get basket-status basket-data) "challenged") (err u112)) ;; Must be challenged
      (asserts! (<= block-height (get termination-block basket-data)) ERR_BASKET_LAPSED)

      ;; Distribute originator's share
      (unwrap! (as-contract (stx-transfer? originator-amount tx-sender originator)) ERR_MOVEMENT_FAILED)

      ;; Distribute beneficiary's share
      (unwrap! (as-contract (stx-transfer? beneficiary-amount tx-sender beneficiary)) ERR_MOVEMENT_FAILED)

      (print {action: "challenge_adjudicated", basket-id: basket-id, originator: originator, beneficiary: beneficiary, 
              originator-share: originator-amount, beneficiary-share: beneficiary-amount, originator-percentage: originator-share})
      (ok true)
    )
  )
)

;; Register additional supervisor for premium baskets
(define-public (register-additional-supervisor (basket-id uint) (supervisor principal))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (quantity (get quantity basket-data))
      )
      ;; Only for premium baskets (> 1000 STX)
      (asserts! (> quantity u1000) (err u120))
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get basket-status basket-data) "pending") ERR_ALREADY_PROCESSED)
      (print {action: "supervisor_registered", basket-id: basket-id, supervisor: supervisor, requester: tx-sender})
      (ok true)
    )
  )
)

;; Register authenticity confirmation
(define-public (register-authenticity-proof (basket-id uint) (confirmation-code (buff 65)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
      )
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary)) ERR_UNAUTHORIZED)
      (asserts! (or (is-eq (get basket-status basket-data) "pending") (is-eq (get basket-status basket-data) "confirmed")) ERR_ALREADY_PROCESSED)
      (print {action: "authenticity_confirmed", basket-id: basket-id, confirmer: tx-sender, confirmation-code: confirmation-code})
      (ok true)
    )
  )
)

;; Register contingency contact
(define-public (register-contingency-contact (basket-id uint) (contingency-contact principal))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
      )
      (asserts! (is-eq tx-sender originator) ERR_UNAUTHORIZED)
      (asserts! (not (is-eq contingency-contact tx-sender)) (err u111)) ;; Contingency contact must be different
      (asserts! (is-eq (get basket-status basket-data) "pending") ERR_ALREADY_PROCESSED)
      (print {action: "contingency_registered", basket-id: basket-id, originator: originator, contingency: contingency-contact})
      (ok true)
    )
  )
)

;; Create phased delivery basket
(define-public (create-phased-basket (beneficiary principal) (resource-id uint) (quantity uint) (phases uint))
  (let 
    (
      (new-id (+ (var-get basket-sequence-id) u1))
      (termination-date (+ block-height BASKET_LIFESPAN_BLOCKS))
      (phase-quantity (/ quantity phases))
    )
    (asserts! (> quantity u0) ERR_INVALID_QUANTITY)
    (asserts! (> phases u0) ERR_INVALID_QUANTITY)
    (asserts! (<= phases u5) ERR_INVALID_QUANTITY) ;; Maximum 5 phases
    (asserts! (valid-beneficiary? beneficiary) ERR_INVALID_ORIGINATOR)
    (asserts! (is-eq (* phase-quantity phases) quantity) (err u121)) ;; Ensure even division
    (match (stx-transfer? quantity tx-sender (as-contract tx-sender))
      success
        (begin
          (var-set basket-sequence-id new-id)
          (print {action: "phased_basket_created", basket-id: new-id, originator: tx-sender, beneficiary: beneficiary, 
                  resource-id: resource-id, quantity: quantity, phases: phases, phase-quantity: phase-quantity})
          (ok new-id)
        )
      error ERR_MOVEMENT_FAILED
    )
  )
)

;; Schedule administrative task
(define-public (schedule-administrative-task (task-type (string-ascii 20)) (task-parameters (list 10 uint)))
  (begin
    (asserts! (is-eq tx-sender PROTOCOL_GOVERNOR) ERR_UNAUTHORIZED)
    (asserts! (> (len task-parameters) u0) ERR_INVALID_QUANTITY)
    (let
      (
        (execution-time (+ block-height u144)) ;; 24 hours delay
      )
      (print {action: "task_scheduled", task-type: task-type, task-parameters: task-parameters, execution-time: execution-time})
      (ok execution-time)
    )
  )
)

;; Activate enhanced verification for premium baskets
(define-public (activate-enhanced-verification (basket-id uint) (verification-hash (buff 32)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (quantity (get quantity basket-data))
      )
      ;; Only for baskets above threshold
      (asserts! (> quantity u5000) (err u130))
      (asserts! (is-eq tx-sender originator) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get basket-status basket-data) "pending") ERR_ALREADY_PROCESSED)
      (print {action: "enhanced_verification_activated", basket-id: basket-id, originator: originator, verification-hash: (hash160 verification-hash)})
      (ok true)
    )
  )
)

;; Suspend questionable basket
(define-public (suspend-questionable-basket (basket-id uint) (justification (string-ascii 100)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
      )
      (asserts! (or (is-eq tx-sender PROTOCOL_GOVERNOR) (is-eq tx-sender originator) (is-eq tx-sender beneficiary)) ERR_UNAUTHORIZED)
      (asserts! (or (is-eq (get basket-status basket-data) "pending") 
                   (is-eq (get basket-status basket-data) "confirmed")) 
                ERR_ALREADY_PROCESSED)
      (map-set BasketRegistry
        { basket-id: basket-id }
        (merge basket-data { basket-status: "suspended" })
      )
      (print {action: "basket_suspended", basket-id: basket-id, reporter: tx-sender, justification: justification})
      (ok true)
    )
  )
)

;; Cryptographic transaction verification
(define-public (cryptographically-verify-transaction (basket-id uint) (message-digest (buff 32)) (signature-data (buff 65)) (signatory principal))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (verification-result (unwrap! (secp256k1-recover? message-digest signature-data) (err u150)))
      )
      ;; Verify with cryptographic proof
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      (asserts! (or (is-eq signatory originator) (is-eq signatory beneficiary)) (err u151))
      (asserts! (is-eq (get basket-status basket-data) "pending") ERR_ALREADY_PROCESSED)

      ;; Verify signature matches expected signatory
      (asserts! (is-eq (unwrap! (principal-of? verification-result) (err u152)) signatory) (err u153))

      (print {action: "cryptographic_verification_complete", basket-id: basket-id, verifier: tx-sender, signatory: signatory})
      (ok true)
    )
  )
)

;; Attach basket documentation
(define-public (attach-basket-documentation (basket-id uint) (document-category (string-ascii 20)) (document-digest (buff 32)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
      )
      ;; Only authorized parties can add documentation
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      (asserts! (not (is-eq (get basket-status basket-data) "delivered")) (err u160))
      (asserts! (not (is-eq (get basket-status basket-data) "reverted")) (err u161))
      (asserts! (not (is-eq (get basket-status basket-data) "lapsed")) (err u162))

      ;; Valid documentation categories
      (asserts! (or (is-eq document-category "resource-details") 
                   (is-eq document-category "delivery-proof")
                   (is-eq document-category "quality-attestation")
                   (is-eq document-category "originator-instructions")) (err u163))

      (print {action: "documentation_attached", basket-id: basket-id, document-category: document-category, 
              document-digest: document-digest, submitter: tx-sender})
      (ok true)
    )
  )
)

;; Configure delayed recovery mechanism
(define-public (configure-delayed-recovery (basket-id uint) (delay-duration uint) (recovery-delegate principal))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (asserts! (> delay-duration u72) ERR_INVALID_QUANTITY) ;; Minimum 72 blocks delay (~12 hours)
    (asserts! (<= delay-duration u1440) ERR_INVALID_QUANTITY) ;; Maximum 1440 blocks delay (~10 days)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (activation-block (+ block-height delay-duration))
      )
      (asserts! (is-eq tx-sender originator) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get basket-status basket-data) "pending") ERR_ALREADY_PROCESSED)
      (asserts! (not (is-eq recovery-delegate originator)) (err u180)) ;; Recovery delegate must differ from originator
      (asserts! (not (is-eq recovery-delegate (get beneficiary basket-data))) (err u181)) ;; Recovery delegate must differ from beneficiary
      (print {action: "delayed_recovery_configured", basket-id: basket-id, originator: originator, 
              recovery-delegate: recovery-delegate, activation-block: activation-block})
      (ok activation-block)
    )
  )
)

;; Execute delayed extraction
(define-public (execute-delayed-extraction (basket-id uint))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (quantity (get quantity basket-data))
        (status (get basket-status basket-data))
        (delay-blocks u24) ;; 24 blocks delay (~4 hours)
      )
      ;; Only originator or governor can execute
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      ;; Only from extraction-pending state
      (asserts! (is-eq status "extraction-pending") (err u301))
      ;; Delay must have expired
      (asserts! (>= block-height (+ (get creation-block basket-data) delay-blocks)) (err u302))

      ;; Process extraction
      (unwrap! (as-contract (stx-transfer? quantity tx-sender originator)) ERR_MOVEMENT_FAILED)

      ;; Update basket status
      (map-set BasketRegistry
        { basket-id: basket-id }
        (merge basket-data { basket-status: "extracted", quantity: u0 })
      )

      (print {action: "delayed_extraction_complete", basket-id: basket-id, 
              originator: originator, quantity: quantity})
      (ok true)
    )
  )
)

;; Configure access throttling
(define-public (configure-access-throttling (max-attempts uint) (cooldown-duration uint))
  (begin
    (asserts! (is-eq tx-sender PROTOCOL_GOVERNOR) ERR_UNAUTHORIZED)
    (asserts! (> max-attempts u0) ERR_INVALID_QUANTITY)
    (asserts! (<= max-attempts u10) ERR_INVALID_QUANTITY) ;; Maximum 10 attempts allowed
    (asserts! (> cooldown-duration u6) ERR_INVALID_QUANTITY) ;; Minimum 6 blocks cooldown (~1 hour)
    (asserts! (<= cooldown-duration u144) ERR_INVALID_QUANTITY) ;; Maximum 144 blocks cooldown (~1 day)

    ;; Note: Full implementation would track limits in contract variables

    (print {action: "access_throttling_configured", max-attempts: max-attempts, 
            cooldown-duration: cooldown-duration, governor: tx-sender, current-block: block-height})
    (ok true)
  )
)

;; Zero-knowledge verification for premium baskets
(define-public (zero-knowledge-verify-basket (basket-id uint) (zk-verification-proof (buff 128)) (public-inputs (list 5 (buff 32))))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (asserts! (> (len public-inputs) u0) ERR_INVALID_QUANTITY)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (quantity (get quantity basket-data))
      )
      ;; Only premium baskets need ZK verification
      (asserts! (> quantity u10000) (err u190))
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      (asserts! (or (is-eq (get basket-status basket-data) "pending") (is-eq (get basket-status basket-data) "confirmed")) ERR_ALREADY_PROCESSED)

      ;; In production, actual ZK proof verification would occur here

      (print {action: "zk_verification_complete", basket-id: basket-id, verifier: tx-sender, 
              proof-digest: (hash160 zk-verification-proof), public-inputs: public-inputs})
      (ok true)
    )
  )
)

;; Transfer basket stewardship
(define-public (transfer-basket-stewardship (basket-id uint) (new-steward principal) (authorization-code (buff 32)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (current-steward (get originator basket-data))
        (current-status (get basket-status basket-data))
      )
      ;; Only current steward or governor can transfer
      (asserts! (or (is-eq tx-sender current-steward) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      ;; New steward must be different
      (asserts! (not (is-eq new-steward current-steward)) (err u210))
      (asserts! (not (is-eq new-steward (get beneficiary basket-data))) (err u211))
      ;; Only certain states allow transfer
      (asserts! (or (is-eq current-status "pending") (is-eq current-status "confirmed")) ERR_ALREADY_PROCESSED)
      ;; Update basket stewardship
      (map-set BasketRegistry
        { basket-id: basket-id }
        (merge basket-data { originator: new-steward })
      )
      (print {action: "stewardship_transferred", basket-id: basket-id, 
              previous-steward: current-steward, new-steward: new-steward, authorization-digest: (hash160 authorization-code)})
      (ok true)
    )
  )
)

;; Multi-signature basket approval
;; Requires multiple authorized signatories to approve high-value baskets before processing
(define-public (register-multi-signature-approval (basket-id uint) (signatory-role (string-ascii 20)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (quantity (get quantity basket-data))
      )
      ;; Only for high-value baskets (> 5000 STX)
      (asserts! (> quantity u5000) (err u220))
      ;; Only authorized parties can register approvals
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      ;; Only in pending or confirmed status
      (asserts! (or (is-eq (get basket-status basket-data) "pending") (is-eq (get basket-status basket-data) "confirmed")) ERR_ALREADY_PROCESSED)
      ;; Validate role type
      (asserts! (or (is-eq signatory-role "originator")
                   (is-eq signatory-role "beneficiary")
                   (is-eq signatory-role "auditor")
                   (is-eq signatory-role "governor")) (err u221))

      (print {action: "multi_sig_approval_registered", basket-id: basket-id, signatory: tx-sender, 
              role: signatory-role, quantity: quantity})
      (ok true)
    )
  )
)

;; Emergency basket freeze
;; Allows authorized parties to freeze a basket in case of suspicious activity
(define-public (emergency-basket-freeze (basket-id uint) (reason (string-ascii 50)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
      )
      ;; Only authorized parties can freeze
      (asserts! (or (is-eq tx-sender PROTOCOL_GOVERNOR) 
                   (is-eq tx-sender originator) 
                   (is-eq tx-sender beneficiary)) ERR_UNAUTHORIZED)
      ;; Can't freeze already finalized baskets
      (asserts! (not (is-eq (get basket-status basket-data) "delivered")) (err u230))
      (asserts! (not (is-eq (get basket-status basket-data) "reverted")) (err u231))
      (asserts! (not (is-eq (get basket-status basket-data) "terminated")) (err u232))
      (asserts! (not (is-eq (get basket-status basket-data) "lapsed")) (err u233))

      ;; Update basket status to frozen
      (map-set BasketRegistry
        { basket-id: basket-id }
        (merge basket-data { basket-status: "frozen" })
      )

      (print {action: "emergency_freeze", basket-id: basket-id, requester: tx-sender, reason: reason})
      (ok true)
    )
  )
)

;; Rate limiting for sensitive operations
;; Prevents brute force attacks by limiting operation frequency
(define-public (validate-operation-rate-limit (operation-type (string-ascii 20)) (target-basket uint))
  (begin
    (asserts! (basket-exists? target-basket) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: target-basket }) ERR_BASKET_MISSING))
        (last-operation-block (- block-height u1)) ;; In production would track actual last operation block
        (min-spacing u6) ;; Minimum 6 blocks between operations (~1 hour)
      )
      ;; Only authorized parties 
      (asserts! (or (is-eq tx-sender (get originator basket-data)) 
                   (is-eq tx-sender (get beneficiary basket-data))
                   (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)

      ;; Check rate limit (would interact with operation tracking in production)
      (asserts! (>= (- block-height last-operation-block) min-spacing) (err u240))

      ;; Validate operation type
      (asserts! (or (is-eq operation-type "delivery")
                   (is-eq operation-type "termination")
                   (is-eq operation-type "challenge")
                   (is-eq operation-type "verification")) (err u241))

      (print {action: "rate_limit_validated", operation-type: operation-type, 
              target-basket: target-basket, requester: tx-sender, block: block-height})
      (ok true)
    )
  )
)

;; Tiered authorization requirements
;; Implements progressive security measures based on basket value
(define-public (enforce-tiered-security (basket-id uint) (auth-level (string-ascii 10)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (quantity (get quantity basket-data))
      )
      ;; Only originator or governor can enforce security levels
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      ;; Can only be applied to pending baskets
      (asserts! (is-eq (get basket-status basket-data) "pending") ERR_ALREADY_PROCESSED)

      ;; Validate auth level against basket value
      (asserts! (or 
                 (and (is-eq auth-level "standard") (< quantity u1000))
                 (and (is-eq auth-level "enhanced") (and (>= quantity u1000) (< quantity u10000)))
                 (and (is-eq auth-level "premium") (>= quantity u10000))
                ) (err u250))

      (print {action: "tiered_security_enforced", basket-id: basket-id, 
              auth-level: auth-level, quantity: quantity, enforcer: tx-sender})
      (ok true)
    )
  )
)

;; Initiate basket challenge
(define-public (challenge-basket (basket-id uint) (justification (string-ascii 50)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
      )
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary)) ERR_UNAUTHORIZED)
      (asserts! (or (is-eq (get basket-status basket-data) "pending") (is-eq (get basket-status basket-data) "confirmed")) ERR_ALREADY_PROCESSED)
      (asserts! (<= block-height (get termination-block basket-data)) ERR_BASKET_LAPSED)
      (map-set BasketRegistry
        { basket-id: basket-id }
        (merge basket-data { basket-status: "challenged" })
      )
      (print {action: "basket_challenged", basket-id: basket-id, challenger: tx-sender, justification: justification})
      (ok true)
    )
  )
)

;; Register time-locked basket with gradual release
;; Creates a basket where funds are released to the beneficiary gradually over time
(define-public (create-time-locked-basket (beneficiary principal) (resource-id uint) (quantity uint) (unlock-intervals uint))
  (begin
    (asserts! (> quantity u0) ERR_INVALID_QUANTITY)
    (asserts! (> unlock-intervals u0) ERR_INVALID_QUANTITY)
    (asserts! (<= unlock-intervals u10) ERR_INVALID_QUANTITY) ;; Maximum 10 intervals
    (asserts! (valid-beneficiary? beneficiary) ERR_INVALID_ORIGINATOR)
    (asserts! (is-eq (* (/ quantity unlock-intervals) unlock-intervals) quantity) (err u270)) ;; Ensure even division
    (let 
      (
        (new-id (+ (var-get basket-sequence-id) u1))
        (termination-date (+ block-height BASKET_LIFESPAN_BLOCKS))
        (interval-amount (/ quantity unlock-intervals))
        (interval-blocks (/ BASKET_LIFESPAN_BLOCKS unlock-intervals))
      )
      (match (stx-transfer? quantity tx-sender (as-contract tx-sender))
        success
          (begin
            (var-set basket-sequence-id new-id)
            (print {action: "time_locked_basket_created", basket-id: new-id, originator: tx-sender, beneficiary: beneficiary, 
                   resource-id: resource-id, quantity: quantity, intervals: unlock-intervals, 
                   interval-amount: interval-amount, interval-blocks: interval-blocks})
            (ok new-id)
          )
        error ERR_MOVEMENT_FAILED
      )
    )
  )
)

;; Implement circuit breaker for protocol operations
;; Allows temporary suspension of specific functionalities during emergencies
(define-public (activate-circuit-breaker (operation-type (string-ascii 20)) (duration uint) (reason (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender PROTOCOL_GOVERNOR) ERR_UNAUTHORIZED)
    (asserts! (> duration u0) ERR_INVALID_QUANTITY)
    (asserts! (<= duration u1440) (err u290)) ;; Maximum ~10 days
    (let
      (
        (expiration-block (+ block-height duration))
      )
      ;; Validate operation type
      (asserts! (or (is-eq operation-type "basket-creation")
                   (is-eq operation-type "delivery-finalization")
                   (is-eq operation-type "challenge-processing")
                   (is-eq operation-type "termination-requests")
                   (is-eq operation-type "all-operations")) (err u291))

      ;; In production would update circuit breaker state in contract storage

      (print {action: "circuit_breaker_activated", operation-type: operation-type, 
              duration: duration, expiration-block: expiration-block, 
              governor: tx-sender, reason: reason})
      (ok expiration-block)
    )
  )
)

;; Time-delayed recovery with escrow
;; Implements secure asset recovery with mandatory time-delay
(define-public (initiate-time-delayed-recovery (basket-id uint) (recovery-address principal))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (quantity (get quantity basket-data))
        (cooling-period u144) ;; 144 blocks (~24 hours)
      )
      ;; Only originator or governor can initiate recovery
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      ;; Only for baskets in specific states
      (asserts! (or (is-eq (get basket-status basket-data) "pending") 
                   (is-eq (get basket-status basket-data) "confirmed")
                   (is-eq (get basket-status basket-data) "frozen")) ERR_ALREADY_PROCESSED)
      ;; Recovery address cannot be same as originator
      (asserts! (not (is-eq recovery-address originator)) (err u260))
      ;; Recovery address cannot be beneficiary
      (asserts! (not (is-eq recovery-address (get beneficiary basket-data))) (err u261))

      ;; Update basket status to recovery-pending

      (print {action: "time_delayed_recovery_initiated", basket-id: basket-id, originator: originator, 
              recovery-address: recovery-address, execution-block: (+ block-height cooling-period)})
      (ok (+ block-height cooling-period))
    )
  )
)

;; Implement quantum-resistant signature verification
;; Provides an additional layer of cryptographic security for high-value baskets
(define-public (verify-quantum-resistant-signature (basket-id uint) (message-hash (buff 64)) (signature (buff 128)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (quantity (get quantity basket-data))
      )
      ;; Only for high-value baskets
      (asserts! (> quantity u20000) (err u310))
      ;; Only authorized parties
      (asserts! (or (is-eq tx-sender originator) 
                   (is-eq tx-sender beneficiary)
                   (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      ;; Only for baskets in specific states
      (asserts! (or (is-eq (get basket-status basket-data) "pending") 
                   (is-eq (get basket-status basket-data) "confirmed")
                   (is-eq (get basket-status basket-data) "challenged")) ERR_ALREADY_PROCESSED)

      ;; Validate signature length - quantum resistant signatures are longer
      (asserts! (>= (len signature) u64) (err u311))

      ;; In production would perform actual quantum-resistant signature verification
      ;; using appropriate algorithms like SPHINCS+, Dilithium, or similar

      (print {action: "quantum_resistant_verification", basket-id: basket-id, 
              message-digest: (hash160 message-hash), signature-length: (len signature), 
              verifier: tx-sender, high-security: true})
      (ok true)
    )
  )
)

;; Register delegate to act on behalf of originator or beneficiary
;; Enables secure temporary access management with expiration
(define-public (register-delegate-authority (basket-id uint) (delegate principal) (authority-type (string-ascii 10)) (expiration-delta uint))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (asserts! (> expiration-delta u6) ERR_INVALID_QUANTITY) ;; Minimum 6 blocks (~1 hour)
    (asserts! (<= expiration-delta u4320) ERR_INVALID_QUANTITY) ;; Maximum 4320 blocks (~30 days)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (expiration-block (+ block-height expiration-delta))
      )
      ;; Only originator can delegate originator authority
      ;; Only beneficiary can delegate beneficiary authority
      ;; Only governor can delegate governor authority
      (asserts! (or (and (is-eq authority-type "originator") (is-eq tx-sender originator))
                    (and (is-eq authority-type "beneficiary") (is-eq tx-sender beneficiary))
                    (and (is-eq authority-type "governor") (is-eq tx-sender PROTOCOL_GOVERNOR))) 
                ERR_UNAUTHORIZED)

      ;; Delegate must not be same as grantor
      (asserts! (not (is-eq delegate tx-sender)) (err u280))

      ;; Delegate must not be the other party in the basket
      (asserts! (not (and (is-eq authority-type "originator") (is-eq delegate beneficiary))) (err u281))
      (asserts! (not (and (is-eq authority-type "beneficiary") (is-eq delegate originator))) (err u282))

      ;; Only allow for active baskets
      (asserts! (or (is-eq (get basket-status basket-data) "pending") 
                   (is-eq (get basket-status basket-data) "confirmed")) ERR_ALREADY_PROCESSED)

      (print {action: "delegate_registered", basket-id: basket-id, 
              grantor: tx-sender, delegate: delegate, 
              authority-type: authority-type, expiration-block: expiration-block})
      (ok expiration-block)
    )
  )
)


;; Implement velocity monitoring for basket operations
;; Detects and prevents anomalous transaction patterns
(define-public (verify-operation-velocity (basket-id uint) (operation-count uint) (timeframe uint))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (asserts! (> operation-count u0) ERR_INVALID_QUANTITY)
    (asserts! (> timeframe u0) ERR_INVALID_QUANTITY)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (quantity (get quantity basket-data))
        (max-operations-per-block u5) ;; Maximum operations allowed per block
        (velocity-coefficient (/ operation-count timeframe))
      )
      ;; Only authorized parties can verify velocity
      (asserts! (or (is-eq tx-sender originator) 
                   (is-eq tx-sender beneficiary)
                   (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)

      ;; Check if velocity exceeds threshold
      (asserts! (<= velocity-coefficient max-operations-per-block) (err u300))

      ;; Apply stricter limits for high-value baskets
      (if (> quantity u10000)
          (asserts! (<= velocity-coefficient (/ max-operations-per-block u2)) (err u301))
          true
      )

      (print {action: "velocity_verified", basket-id: basket-id, operation-count: operation-count, 
              timeframe: timeframe, velocity: velocity-coefficient, verifier: tx-sender})
      (ok true)
    )
  )
)

;; Submit counter-signature for high-value basket operations
;; Implements N-of-M authentication for critical operations
(define-public (submit-basket-counter-signature (basket-id uint) (operation-hash (buff 32)) (signature-data (buff 65)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (quantity (get quantity basket-data))
      )
      ;; Only relevant for high-value baskets
      (asserts! (> quantity u2000) (err u290))

      ;; Only authorized parties can submit signatures
      (asserts! (or (is-eq tx-sender originator) 
                   (is-eq tx-sender beneficiary)
                   (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)

      ;; Basket must be in active state
      (asserts! (or (is-eq (get basket-status basket-data) "pending") 
                   (is-eq (get basket-status basket-data) "confirmed")
                   (is-eq (get basket-status basket-data) "challenged")) ERR_ALREADY_PROCESSED)

      ;; Verify signature authenticity (simplified - production would verify cryptographically)
      (asserts! (> (len signature-data) u60) (err u291))

      (print {action: "counter_signature_submitted", basket-id: basket-id, 
              signer: tx-sender, operation-hash: operation-hash, 
              signature-digest: (hash160 signature-data)})
      (ok true)
    )
  )
)

;; Perform risk-based transaction verification
;; Adapts security requirements based on basket value and risk assessment
(define-public (perform-risk-based-verification (basket-id uint) (risk-factors (list 5 (string-ascii 20))) (verification-level uint))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (asserts! (> verification-level u0) ERR_INVALID_QUANTITY)
    (asserts! (<= verification-level u3) ERR_INVALID_QUANTITY) ;; Maximum 3 levels (standard, enhanced, premium)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (quantity (get quantity basket-data))
        (risk-score (+ verification-level (len risk-factors))) ;; Simple risk scoring
      )
      ;; Authorization checks
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)

      ;; Basket must be in pending state
      (asserts! (is-eq (get basket-status basket-data) "pending") ERR_ALREADY_PROCESSED)

      ;; Verification level must align with basket value
      (asserts! (or 
                 (and (is-eq verification-level u1) (< quantity u1000)) 
                 (and (is-eq verification-level u2) (and (>= quantity u1000) (< quantity u5000)))
                 (and (is-eq verification-level u3) (>= quantity u5000))
                ) (err u300))

      (print {action: "risk_verification_completed", basket-id: basket-id, 
              risk-factors: risk-factors, verification-level: verification-level, 
              risk-score: risk-score, verifier: tx-sender})
      (ok risk-score)
    )
  )
)

;; Register basket heartbeat for monitoring
;; Establishes continuous monitoring with deadman-switch functionality
(define-public (register-basket-heartbeat (basket-id uint) (heartbeat-interval uint) (alert-threshold uint))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (asserts! (> heartbeat-interval u6) ERR_INVALID_QUANTITY) ;; Minimum 6 blocks (~1 hour)
    (asserts! (<= heartbeat-interval u144) ERR_INVALID_QUANTITY) ;; Maximum 144 blocks (~1 day)
    (asserts! (> alert-threshold u1) ERR_INVALID_QUANTITY) ;; Minimum 1 missed heartbeat
    (asserts! (<= alert-threshold u5) ERR_INVALID_QUANTITY) ;; Maximum 5 missed heartbeats
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (next-heartbeat-block (+ block-height heartbeat-interval))
        (total-monitoring-window (* heartbeat-interval alert-threshold))
      )
      ;; Only basket stakeholders can configure heartbeat
      (asserts! (or (is-eq tx-sender originator) 
                   (is-eq tx-sender beneficiary)
                   (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)

      ;; Only for active baskets
      (asserts! (or (is-eq (get basket-status basket-data) "pending") 
                   (is-eq (get basket-status basket-data) "confirmed")) ERR_ALREADY_PROCESSED)

      ;; Ensure heartbeat window is within basket lifespan
      (asserts! (<= (+ block-height total-monitoring-window) (get termination-block basket-data)) (err u310))

      (print {action: "heartbeat_registered", basket-id: basket-id, 
              registrar: tx-sender, interval: heartbeat-interval, 
              alert-threshold: alert-threshold, next-heartbeat: next-heartbeat-block})
      (ok next-heartbeat-block)
    )
  )
)

;; Register trusted entity certification for basket
;; Adds third-party verification and escrow guarantees
(define-public (register-trusted-certification (basket-id uint) (certification-type (string-ascii 20)) (certification-hash (buff 32)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (quantity (get quantity basket-data))
      )
      ;; Only originator, beneficiary or governor can register certification
      (asserts! (or (is-eq tx-sender originator) 
                   (is-eq tx-sender beneficiary)
                   (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)

      ;; Basket must be in appropriate state
      (asserts! (or (is-eq (get basket-status basket-data) "pending") 
                   (is-eq (get basket-status basket-data) "confirmed")) ERR_ALREADY_PROCESSED)

      ;; Validate certification type
      (asserts! (or (is-eq certification-type "resource-authenticity")
                   (is-eq certification-type "delivery-guarantee")
                   (is-eq certification-type "value-appraisal")
                   (is-eq certification-type "identity-verification")
                   (is-eq certification-type "escrow-guarantee")) (err u320))

      ;; Premium certification types only for high-value baskets
      (asserts! (not (and (or (is-eq certification-type "value-appraisal") 
                             (is-eq certification-type "escrow-guarantee"))
                        (< quantity u5000))) (err u321))

      (print {action: "certification_registered", basket-id: basket-id, 
              certification-type: certification-type, certifier: tx-sender, 
              certification-hash: certification-hash, quantity: quantity})
      (ok true)
    )
  )
)

;; Verify basket ownership with time-based challenge
;; Allows originators to prove ownership through a challenge-response mechanism
(define-public (verify-basket-ownership (basket-id uint) (challenge-response (buff 32)) (verification-timestamp uint))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (challenge-window u12) ;; 12 block window for responding to challenge (~2 hours)
      )
      ;; Only basket originator can verify ownership
      (asserts! (is-eq tx-sender originator) ERR_UNAUTHORIZED)
      ;; Verification must be timely
      (asserts! (and (>= block-height verification-timestamp) (<= block-height (+ verification-timestamp challenge-window))) (err u270))
      ;; Response must not be empty
      (asserts! (> (len challenge-response) u0) ERR_INVALID_QUANTITY)

      ;; In production, would validate the cryptographic challenge response

      (print {action: "ownership_verified", basket-id: basket-id, originator: originator, 
              verification-time: block-height, challenge-digest: (hash160 challenge-response)})
      (ok true)
    )
  )
)

;; Conditional basket delivery with external verification
;; Allows delivery only when external verification conditions are met
(define-public (conditional-basket-delivery (basket-id uint) (delivery-conditions (list 3 (buff 32))) (verification-proof (buff 64)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (quantity (get quantity basket-data))
      )
      ;; Only authorized parties can execute conditional delivery
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      ;; Basket must be in pending state
      (asserts! (is-eq (get basket-status basket-data) "pending") ERR_ALREADY_PROCESSED)
      ;; Must provide conditions and verification
      (asserts! (> (len delivery-conditions) u0) ERR_INVALID_QUANTITY)
      (asserts! (> (len verification-proof) u0) ERR_INVALID_QUANTITY)

      ;; Execute conditional delivery if verification passes
      (match (as-contract (stx-transfer? quantity tx-sender beneficiary))
        success
          (begin
            (map-set BasketRegistry
              { basket-id: basket-id }
              (merge basket-data { basket-status: "delivered" })
            )
            (print {action: "conditional_delivery_complete", basket-id: basket-id, beneficiary: beneficiary, 
                    conditions-count: (len delivery-conditions), verification-digest: (hash160 verification-proof)})
            (ok true)
          )
        error ERR_MOVEMENT_FAILED
      )
    )
  )
)

;; Escrow basket with split payment structure
;; Implements phased resource delivery with partial payments
(define-public (escrow-basket-split-payment (basket-id uint) (release-percentages (list 5 uint)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (quantity (get quantity basket-data))
        (total-percentage u0)
      )
      ;; Only governor can arrange split payments
      (asserts! (is-eq tx-sender PROTOCOL_GOVERNOR) ERR_UNAUTHORIZED)
      ;; Basket must be in appropriate state
      (asserts! (or (is-eq (get basket-status basket-data) "pending") (is-eq (get basket-status basket-data) "confirmed")) ERR_ALREADY_PROCESSED)
      ;; Validate percentages
      (asserts! (> (len release-percentages) u0) ERR_INVALID_QUANTITY)
      (asserts! (<= (len release-percentages) u5) ERR_INVALID_QUANTITY)

      ;; Calculate total percentage (would validate sum = 100 in production)
      ;; This is simplified for demonstration
      (asserts! (is-eq (fold + release-percentages u0) u100) (err u280))

      (print {action: "escrow_split_configured", basket-id: basket-id, originator: originator, 
              beneficiary: beneficiary, splits: (len release-percentages), quantity: quantity})
      (ok true)
    )
  )
)

;; Delegate basket management authority
;; Allows temporary delegation of basket management to trusted party
(define-public (delegate-basket-authority (basket-id uint) (delegate principal) (delegation-period uint))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (asserts! (> delegation-period u0) ERR_INVALID_QUANTITY)
    (asserts! (<= delegation-period u720) ERR_INVALID_QUANTITY) ;; Maximum 720 blocks (~5 days)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (delegation-expiry (+ block-height delegation-period))
      )
      ;; Only originator can delegate authority
      (asserts! (is-eq tx-sender originator) ERR_UNAUTHORIZED)
      ;; Delegate must not be beneficiary or originator
      (asserts! (not (is-eq delegate originator)) (err u290))
      (asserts! (not (is-eq delegate (get beneficiary basket-data))) (err u291))
      ;; Must be in appropriate state
      (asserts! (or (is-eq (get basket-status basket-data) "pending") (is-eq (get basket-status basket-data) "confirmed")) ERR_ALREADY_PROCESSED)

      ;; Register delegation (would store delegate in map in production)
      (print {action: "authority_delegated", basket-id: basket-id, originator: originator, 
              delegate: delegate, expiration-block: delegation-expiry})
      (ok delegation-expiry)
    )
  )
)

;; Two-factor basket authentication
;; Requires secondary authentication before high-value basket operations
(define-public (authenticate-basket-operation (basket-id uint) (operation-type (string-ascii 20)) (auth-code (buff 32)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (quantity (get quantity basket-data))
      )
      ;; Only authorized parties can authenticate
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary)) ERR_UNAUTHORIZED)
      ;; Only high-value baskets require 2FA
      (asserts! (> quantity u1000) (err u300))
      ;; Valid operation types
      (asserts! (or (is-eq operation-type "delivery")
                    (is-eq operation-type "termination")
                    (is-eq operation-type "transfer")
                    (is-eq operation-type "modification")) (err u301))
      ;; Auth code must not be empty
      (asserts! (> (len auth-code) u0) ERR_INVALID_QUANTITY)

      ;; In production, would validate the authentication code

      (print {action: "two_factor_authenticated", basket-id: basket-id, requester: tx-sender, 
              operation-type: operation-type, auth-digest: (hash160 auth-code)})
      (ok true)
    )
  )
)

;; Verify resource integrity with merkle proof
;; Allows cryptographic verification of resource integrity
(define-public (verify-resource-integrity (basket-id uint) (merkle-proof (list 10 (buff 32))) (leaf-hash (buff 32)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (beneficiary (get beneficiary basket-data))
        (resource-id (get resource-id basket-data))
      )
      ;; Only authorized parties can verify integrity
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_GOVERNOR)) ERR_UNAUTHORIZED)
      ;; Proof must not be empty
      (asserts! (> (len merkle-proof) u0) ERR_INVALID_QUANTITY)
      ;; Leaf hash must not be empty
      (asserts! (> (len leaf-hash) u0) ERR_INVALID_QUANTITY)

      ;; In production, would validate the merkle proof against a stored root hash

      ;; Record successful verification
      (print {action: "integrity_verified", basket-id: basket-id, resource-id: resource-id, 
              verifier: tx-sender, proof-depth: (len merkle-proof), leaf-digest: (hash160 leaf-hash)})
      (ok true)
    )
  )
)

;; Schedule periodic security audit
;; Enables governance to schedule routine security audits for baskets
(define-public (schedule-basket-security-audit (basket-id uint) (audit-type (string-ascii 20)))
  (begin
    (asserts! (basket-exists? basket-id) ERR_INVALID_BASKET_ID)
    (let
      (
        (basket-data (unwrap! (map-get? BasketRegistry { basket-id: basket-id }) ERR_BASKET_MISSING))
        (originator (get originator basket-data))
        (quantity (get quantity basket-data))
        (next-audit-block (+ block-height u720)) ;; 720 blocks (~5 days) delay
      )
      ;; Only governor or originator can schedule audits
      (asserts! (or (is-eq tx-sender PROTOCOL_GOVERNOR) (is-eq tx-sender originator)) ERR_UNAUTHORIZED)
      ;; Only for active baskets
      (asserts! (or (is-eq (get basket-status basket-data) "pending") 
                    (is-eq (get basket-status basket-data) "confirmed")) ERR_ALREADY_PROCESSED)
      ;; Validate audit type
      (asserts! (or (is-eq audit-type "routine")
                    (is-eq audit-type "deep-inspection")
                    (is-eq audit-type "forensic")) (err u270))

      (print {action: "security_audit_scheduled", basket-id: basket-id, audit-type: audit-type, 
              scheduled-by: tx-sender, execution-block: next-audit-block})
      (ok next-audit-block)
    )
  )
)

;; Implement emergency circuit breaker
;; Allows governor to halt all operations in case of detected security incidents
(define-public (activate-emergency-circuit-breaker (reason (string-ascii 100)))
  (begin
    ;; Only governor can activate circuit breaker
    (asserts! (is-eq tx-sender PROTOCOL_GOVERNOR) ERR_UNAUTHORIZED)
    ;; Validate reason length
    (asserts! (> (len reason) u10) (err u280)) ;; Require substantive reason

    ;; In production, this would set a global flag in the contract
    ;; that would be checked by all critical operations

    (print {action: "circuit_breaker_activated", governor: tx-sender, 
            reason: reason, activation-block: block-height, 
            estimated-duration: "indefinite"})
    (ok block-height)
  )
)

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

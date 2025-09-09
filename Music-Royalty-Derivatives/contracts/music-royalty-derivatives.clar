;; Music Royalty Derivatives - Trade future music earnings as financial instruments
;; This contract enables the creation and trading of derivative instruments backed by music royalties

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-DERIVATIVE-NOT-FOUND (err u103))
(define-constant ERR-EXPIRED-DERIVATIVE (err u104))
(define-constant ERR-INVALID-MATURITY (err u105))
(define-constant ERR-SETTLEMENT-FAILED (err u106))
(define-constant ERR-ALREADY-SETTLED (err u107))
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u108))

(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-COLLATERAL-RATIO u150) ;; 150% collateralization
(define-constant SETTLEMENT-FEE u50) ;; 0.5% fee

;; Data structures
(define-map derivatives
  { derivative-id: uint }
  {
    issuer: principal,
    music-asset-id: (string-ascii 50),
    underlying-value: uint,
    strike-price: uint,
    maturity-block: uint,
    collateral-amount: uint,
    total-supply: uint,
    is-settled: bool,
    settlement-price: uint,
    created-at: uint
  }
)

(define-map derivative-balances
  { derivative-id: uint, holder: principal }
  { balance: uint }
)

(define-map user-positions
  { user: principal, derivative-id: uint }
  {
    long-position: uint,
    short-position: uint,
    collateral-locked: uint
  }
)

(define-map music-assets
  { asset-id: (string-ascii 50) }
  {
    artist: principal,
    title: (string-ascii 100),
    total-royalties: uint,
    verified: bool,
    royalty-rate: uint ;; basis points
  }
)

(define-map royalty-payments
  { asset-id: (string-ascii 50), period: uint }
  { amount: uint, timestamp: uint }
)

(define-data-var next-derivative-id uint u1)
(define-data-var total-volume uint u0)
(define-data-var platform-fees uint u0)

;; Oracle data for settlement prices
(define-map settlement-oracles
  { derivative-id: uint }
  { oracle: principal, price: uint, timestamp: uint }
)

;; Register a new music asset
(define-public (register-music-asset 
  (asset-id (string-ascii 50))
  (title (string-ascii 100))
  (royalty-rate uint))
  
  (begin
    (asserts! (> royalty-rate u0) ERR-INVALID-AMOUNT)
    (asserts! (<= royalty-rate u10000) ERR-INVALID-AMOUNT) ;; Max 100%
    
    (map-set music-assets
      { asset-id: asset-id }
      {
        artist: tx-sender,
        title: title,
        total-royalties: u0,
        verified: false,
        royalty-rate: royalty-rate
      }
    )
    (ok asset-id)
  )
)

;; Create a new derivative instrument
(define-public (create-derivative
  (music-asset-id (string-ascii 50))
  (underlying-value uint)
  (strike-price uint)
  (maturity-blocks uint)
  (total-supply uint)
  (collateral-amount uint))
  
  (let (
    (derivative-id (var-get next-derivative-id))
    (maturity-block (+ block-height maturity-blocks))
    (required-collateral (/ (* underlying-value MIN-COLLATERAL-RATIO) u100))
  )
    
    (asserts! (> underlying-value u0) ERR-INVALID-AMOUNT)
    (asserts! (> strike-price u0) ERR-INVALID-AMOUNT)
    (asserts! (> maturity-blocks u0) ERR-INVALID-MATURITY)
    (asserts! (> total-supply u0) ERR-INVALID-AMOUNT)
    (asserts! (>= collateral-amount required-collateral) ERR-INSUFFICIENT-COLLATERAL)
    (asserts! (is-some (map-get? music-assets { asset-id: music-asset-id })) ERR-DERIVATIVE-NOT-FOUND)
    
    ;; Transfer collateral from issuer
    (try! (stx-transfer? collateral-amount tx-sender (as-contract tx-sender)))
    
    (map-set derivatives
      { derivative-id: derivative-id }
      {
        issuer: tx-sender,
        music-asset-id: music-asset-id,
        underlying-value: underlying-value,
        strike-price: strike-price,
        maturity-block: maturity-block,
        collateral-amount: collateral-amount,
        total-supply: total-supply,
        is-settled: false,
        settlement-price: u0,
        created-at: block-height
      }
    )
    
    ;; Give issuer the derivative tokens
    (map-set derivative-balances
      { derivative-id: derivative-id, holder: tx-sender }
      { balance: total-supply }
    )
    
    (var-set next-derivative-id (+ derivative-id u1))
    (ok derivative-id)
  )
)

;; Transfer derivative tokens
(define-public (transfer-derivative
  (derivative-id uint)
  (amount uint)
  (recipient principal))
  
  (let (
    (sender-balance (default-to u0 (get balance (map-get? derivative-balances 
      { derivative-id: derivative-id, holder: tx-sender }))))
  )
    
    (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Update sender balance
    (map-set derivative-balances
      { derivative-id: derivative-id, holder: tx-sender }
      { balance: (- sender-balance amount) }
    )
    
    ;; Update recipient balance
    (let (
      (recipient-balance (default-to u0 (get balance (map-get? derivative-balances 
        { derivative-id: derivative-id, holder: recipient }))))
    )
      (map-set derivative-balances
        { derivative-id: derivative-id, holder: recipient }
        { balance: (+ recipient-balance amount) }
      )
    )
    
    (ok true)
  )
)

;; Record royalty payment for settlement calculation
(define-public (record-royalty-payment
  (asset-id (string-ascii 50))
  (period uint)
  (amount uint))
  
  (let (
    (asset-info (unwrap! (map-get? music-assets { asset-id: asset-id }) ERR-DERIVATIVE-NOT-FOUND))
  )
    
    (asserts! (is-eq tx-sender (get artist asset-info)) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    (map-set royalty-payments
      { asset-id: asset-id, period: period }
      { amount: amount, timestamp: block-height }
    )
    
    ;; Update total royalties for the asset
    (map-set music-assets
      { asset-id: asset-id }
      (merge asset-info { total-royalties: (+ (get total-royalties asset-info) amount) })
    )
    
    (ok true)
  )
)

;; Settle derivative at maturity
(define-public (settle-derivative
  (derivative-id uint)
  (settlement-price uint))
  
  (let (
    (derivative-info (unwrap! (map-get? derivatives { derivative-id: derivative-id }) ERR-DERIVATIVE-NOT-FOUND))
  )
    
    (asserts! (>= block-height (get maturity-block derivative-info)) ERR-INVALID-MATURITY)
    (asserts! (not (get is-settled derivative-info)) ERR-ALREADY-SETTLED)
    (asserts! (or (is-eq tx-sender (get issuer derivative-info)) 
                  (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    
    ;; Record settlement oracle data
    (map-set settlement-oracles
      { derivative-id: derivative-id }
      { oracle: tx-sender, price: settlement-price, timestamp: block-height }
    )
    
    ;; Mark as settled
    (map-set derivatives
      { derivative-id: derivative-id }
      (merge derivative-info { 
        is-settled: true,
        settlement-price: settlement-price 
      })
    )
    
    (ok true)
  )
)

;; Exercise derivative position
(define-public (exercise-derivative
  (derivative-id uint)
  (amount uint))
  
  (let (
    (derivative-info (unwrap! (map-get? derivatives { derivative-id: derivative-id }) ERR-DERIVATIVE-NOT-FOUND))
    (holder-balance (default-to u0 (get balance (map-get? derivative-balances 
      { derivative-id: derivative-id, holder: tx-sender }))))
  )
    
    (asserts! (get is-settled derivative-info) ERR-SETTLEMENT-FAILED)
    (asserts! (>= holder-balance amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    (let (
      (settlement-price (get settlement-price derivative-info))
      (strike-price (get strike-price derivative-info))
      (payout (if (> settlement-price strike-price)
                (/ (* amount (- settlement-price strike-price)) (get total-supply derivative-info))
                u0))
      (fee (/ (* payout SETTLEMENT-FEE) u10000))
      (net-payout (- payout fee))
    )
      
      ;; Update holder balance
      (map-set derivative-balances
        { derivative-id: derivative-id, holder: tx-sender }
        { balance: (- holder-balance amount) }
      )
      
      ;; Transfer payout if positive
      (if (> net-payout u0)
        (try! (as-contract (stx-transfer? net-payout tx-sender tx-sender)))
        (ok true)
      )
      
      ;; Record platform fee
      (var-set platform-fees (+ (var-get platform-fees) fee))
      
      (ok net-payout)
    )
  )
)

;; Get derivative information
(define-read-only (get-derivative-info (derivative-id uint))
  (map-get? derivatives { derivative-id: derivative-id })
)

;; Get user's derivative balance
(define-read-only (get-derivative-balance (derivative-id uint) (holder principal))
  (default-to u0 (get balance (map-get? derivative-balances 
    { derivative-id: derivative-id, holder: holder })))
)

;; Get music asset information
(define-read-only (get-music-asset (asset-id (string-ascii 50)))
  (map-get? music-assets { asset-id: asset-id })
)

;; Get royalty payment for specific period
(define-read-only (get-royalty-payment (asset-id (string-ascii 50)) (period uint))
  (map-get? royalty-payments { asset-id: asset-id, period: period })
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-derivatives: (var-get next-derivative-id),
    total-volume: (var-get total-volume),
    platform-fees: (var-get platform-fees)
  }
)

;; Administrative functions
(define-public (verify-music-asset (asset-id (string-ascii 50)))
  (let (
    (asset-info (unwrap! (map-get? music-assets { asset-id: asset-id }) ERR-DERIVATIVE-NOT-FOUND))
  )
    
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    (map-set music-assets
      { asset-id: asset-id }
      (merge asset-info { verified: true })
    )
    
    (ok true)
  )
)

;; Withdraw platform fees (owner only)
(define-public (withdraw-platform-fees (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= amount (var-get platform-fees)) ERR-INSUFFICIENT-BALANCE)
    
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER)))
    (var-set platform-fees (- (var-get platform-fees) amount))
    
    (ok true)
  )
)
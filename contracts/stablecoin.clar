;; ============ Constants ============
(define-constant collateral-ratio u150) ;; 150%
(define-constant precision u100)        ;; For percentage math
(define-constant minimum-collateral u1000000) ;; Minimum 1 STX
(define-constant oracle 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM) ;; Oracle address

;; ============ Global Variables ============
(define-data-var stx-price uint u100) ;; STX/USD price in cents, e.g. $1.25 = u125

;; ============ Storage ============
(define-map cdp 
  { user: principal }
  { collateral: uint, minted: uint })

;; ============ Stablecoin Token ============
(define-fungible-token dUSD)

;; ============ Admin Functions ============

(define-public (set-stx-price (price uint))
  (begin
    (asserts! (is-eq tx-sender oracle) (err u100)) ;; Only oracle
    (asserts! (> price u0) (err u107)) ;; Price must be positive
    (ok (var-set stx-price price))
  )
)

;; ============ Mint dUSD ============
(define-public (mint-dusd (collateral-amount uint) (dusd-to-mint uint))
  (let (
    (price (var-get stx-price))
    (required-collateral (* dusd-to-mint collateral-ratio))
    (actual-collateral (* collateral-amount price))
  )
    (begin
      (asserts! (>= actual-collateral (* required-collateral precision)) (err u101))
      (asserts! (>= collateral-amount minimum-collateral) (err u106))
      (try! (stx-transfer? collateral-amount tx-sender (as-contract tx-sender)))
      (try! (ft-mint? dUSD dusd-to-mint tx-sender))
      (map-set cdp { user: tx-sender } { collateral: collateral-amount, minted: dusd-to-mint })
      (ok true)
    )
  )
)

;; ============ Burn dUSD & Redeem STX ============
(define-public (burn-and-redeem (burn-amount uint))
  (let ((user-cdp (map-get? cdp { user: tx-sender })))
    (match user-cdp
      cdp-data (let (
          (coll (get collateral cdp-data))
          (minted (get minted cdp-data)))
        (begin
          (asserts! (<= burn-amount minted) (err u102))
          (let (
            (coll-to-release (/ (* burn-amount coll) minted))
            (new-collateral (- coll coll-to-release))
            (new-minted (- minted burn-amount))
          )
            (try! (ft-burn? dUSD burn-amount tx-sender))
            (try! (stx-transfer? coll-to-release (as-contract tx-sender) tx-sender))
            (map-set cdp { user: tx-sender } { collateral: new-collateral, minted: new-minted })
            (ok true)
          )
        ))
      (err u103)
    )
  )
)

;; ============ Liquidation ============
(define-public (liquidate (target principal))
  (let (
    (cdp-data (map-get? cdp { user: target }))
    (price (var-get stx-price))
  )
    (match cdp-data
      data (let (
          (coll (get collateral data))
          (minted (get minted data))
          (coll-value (* coll price))
          (required-value (* minted collateral-ratio))
        )
          (if (< coll-value (* required-value precision))
            (begin
              (try! (ft-burn? dUSD minted tx-sender))
              (try! (stx-transfer? coll (as-contract tx-sender) tx-sender))
              (map-delete cdp { user: target })
              (ok true)
            )
            (err u104) ;; Still healthy
          )
        )
      (err u105) ;; No CDP
    )
  )
)
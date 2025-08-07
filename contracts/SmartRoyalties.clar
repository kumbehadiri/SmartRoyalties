;; SmartRoyalties - Creative work royalty platform
;; Handles royalty distribution and rights management for creative works

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-invalid-percentage (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-adjustment-rule (err u109))
(define-constant err-rate-below-minimum (err u110))
(define-constant err-rate-above-maximum (err u111))

;; Data Variables
(define-data-var platform-fee uint u50) ;; 5% platform fee (represented as basis points)

;; Data Maps
(define-map creative-works 
    principal 
    {
        title: (string-ascii 50),
        description: (string-ascii 200),
        creation-date: uint,
        royalty-percentage: uint,
        total-earnings: uint
    }
)

(define-map rights-holders
    {work-owner: principal, holder: principal}
    {
        percentage: uint,
        earnings: uint
    }
)

(define-map usage-tracking
    {work-owner: principal, user: principal}
    {
        last-payment: uint,
        total-paid: uint,
        license-expiry: uint
    }
)

;; Public Functions

;; Register a new creative work
(define-public (register-work (title (string-ascii 50)) (description (string-ascii 200)) (royalty-percentage uint))
    (let
        ((work-exists (get title (map-get? creative-works tx-sender))))
        (asserts! (is-none work-exists) err-already-registered)
        (asserts! (<= royalty-percentage u1000) err-invalid-percentage) ;; Max 100% (1000 basis points)
        (ok (map-set creative-works tx-sender
            {
                title: title,
                description: description,
                creation-date: stacks-block-height,
                royalty-percentage: royalty-percentage,
                total-earnings: u0
            }
        ))
    )
)

;; Add rights holder for a work
(define-public (add-rights-holder (work-owner principal) (holder principal) (percentage uint))
    (let
        ((work (map-get? creative-works work-owner)))
        (asserts! (is-eq tx-sender work-owner) err-unauthorized)
        (asserts! (is-some work) err-not-found)
        (asserts! (<= percentage u1000) err-invalid-percentage)
        (ok (map-set rights-holders {work-owner: work-owner, holder: holder}
            {
                percentage: percentage,
                earnings: u0
            }
        ))
    )
)

;; Process royalty payment
(define-public (pay-royalty (work-owner principal) (amount uint))
    (let
        ((work (unwrap! (map-get? creative-works work-owner) err-not-found))
         (platform-cut (/ (* amount (var-get platform-fee)) u1000))
         (creator-amount (- amount platform-cut)))
        
        ;; Transfer platform fee
        (try! (stx-transfer? platform-cut tx-sender contract-owner))
        
        ;; Transfer creator payment
        (try! (stx-transfer? creator-amount tx-sender work-owner))
        
        ;; Update usage tracking
        (map-set usage-tracking {work-owner: work-owner, user: tx-sender}
            {
                last-payment: stacks-block-height,
                total-paid: (+ amount (default-to u0 (get total-paid (map-get? usage-tracking {work-owner: work-owner, user: tx-sender})))),
                license-expiry: (+ stacks-block-height u1440) ;; 1 day license
            }
        )
        
        ;; Update work earnings
        (map-set creative-works work-owner
            (merge work {total-earnings: (+ (get total-earnings work) amount)})
        )
        
        (ok true)
    )
)

;; Read-only Functions

;; Get work details
(define-read-only (get-work-details (owner principal))
    (ok (map-get? creative-works owner))
)

;; Get rights holder details
(define-read-only (get-rights-holder-details (work-owner principal) (holder principal))
    (ok (map-get? rights-holders {work-owner: work-owner, holder: holder}))
)

;; Get usage details
(define-read-only (get-usage-details (work-owner principal) (user principal))
    (ok (map-get? usage-tracking {work-owner: work-owner, user: user}))
)

;; Check if license is valid
(define-read-only (is-license-valid (work-owner principal) (user principal))
    (let
        ((usage (map-get? usage-tracking {work-owner: work-owner, user: user})))
        (if (is-some usage)
            (ok (< stacks-block-height (get license-expiry (unwrap-panic usage))))
            (ok false)
        )
    )
)

;; Admin Functions

;; Update platform fee
(define-public (update-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee u1000) err-invalid-percentage)
        (ok (var-set platform-fee new-fee))
    )
)

(define-map work-categories
    principal
    (list 10 (string-ascii 20))
)

(define-public (add-work-categories (categories (list 10 (string-ascii 20))))
    (let ((work (unwrap! (map-get? creative-works tx-sender) err-not-found)))
        (ok (map-set work-categories tx-sender categories))
    )
)

(define-read-only (get-work-categories (owner principal))
    (ok (map-get? work-categories owner))
)


(define-map pricing-tiers
    principal
    {
        basic: uint,
        premium: uint,
        enterprise: uint
    }
)

(define-public (set-work-pricing (basic uint) (premium uint) (enterprise uint))
    (let ((work (unwrap! (map-get? creative-works tx-sender) err-not-found)))
        (ok (map-set pricing-tiers tx-sender {
            basic: basic,
            premium: premium,
            enterprise: enterprise
        }))
    )
)

(define-public (pay-tiered-royalty (work-owner principal) (tier (string-ascii 10)))
    (let (
        (prices (unwrap! (map-get? pricing-tiers work-owner) err-not-found))
        (amount (if (is-eq tier "basic")
            (get basic prices)
            (if (is-eq tier "premium")
                (get premium prices)
                (get enterprise prices))))
    )
        (try! (pay-royalty work-owner amount))
        (ok true)
    )
)


(define-map revenue-pools
    (string-ascii 50)
    {
        members: (list 50 principal),
        share-percentages: (list 50 uint),
        total-earnings: uint
    }
)

(define-public (create-revenue-pool (pool-name (string-ascii 50)) (members (list 50 principal)) (percentages (list 50 uint)))
    (ok (map-set revenue-pools pool-name {
        members: members,
        share-percentages: percentages,
        total-earnings: u0
    }))
)

(define-public (distribute-pool-earnings (pool-name (string-ascii 50)) (amount uint))
    (let ((pool (unwrap! (map-get? revenue-pools pool-name) err-not-found)))
        (try! (stx-transfer? amount tx-sender contract-owner))
        (ok (map-set revenue-pools pool-name 
            (merge pool {total-earnings: (+ (get total-earnings pool) amount)})))
    )
)


(define-map work-versions
    { owner: principal, version: uint }
    {
        hash: (string-ascii 64),
        timestamp: uint,
        changes: (string-ascii 200)
    }
)

(define-public (add-work-version (hash (string-ascii 64)) (changes (string-ascii 200)))
    (let (
        (work (unwrap! (map-get? creative-works tx-sender) err-not-found))
        (version-count (unwrap! (get-last-version tx-sender) err-not-found))
    )
        (ok (map-set work-versions 
            {owner: tx-sender, version: (+ version-count u1)}
            {hash: hash, timestamp: stacks-block-height, changes: changes}))
    )
)

(define-read-only (get-last-version (owner principal))
    (ok (default-to u0 (get timestamp (map-get? work-versions {owner: owner, version: u1}))))
)


(define-map subscriptions
    { subscriber: principal, creator: principal }
    {
        start-height: uint,
        end-height: uint,
        subscription-type: (string-ascii 10)
    }
)

(define-public (create-subscription (creator principal) (duration uint) (sub-type (string-ascii 10)))
    (ok (map-set subscriptions 
        {subscriber: tx-sender, creator: creator}
        {
            start-height: stacks-block-height,
            end-height: (+ stacks-block-height duration),
            subscription-type: sub-type
        }))
)

(define-read-only (check-subscription (subscriber principal) (creator principal))
    (let ((sub (map-get? subscriptions {subscriber: subscriber, creator: creator})))
        (ok (and (is-some sub)
            (< stacks-block-height (get end-height (unwrap-panic sub)))))
    )
)


(define-map collaborations
    (string-ascii 50)
    {
        creators: (list 10 principal),
        split-percentages: (list 10 uint),
        work-status: (string-ascii 10)
    }
)

(define-public (create-collaboration (collab-id (string-ascii 50)) (creators (list 10 principal)) (splits (list 10 uint)))
    (ok (map-set collaborations collab-id {
        creators: creators,
        split-percentages: splits,
        work-status: "active"
    }))
)

(define-public (update-collab-status (collab-id (string-ascii 50)) (status (string-ascii 10)))
    (let ((collab (unwrap! (map-get? collaborations collab-id) err-not-found)))
        (ok (map-set collaborations collab-id 
            (merge collab {work-status: status})))
    )
)

(define-map work-analytics
    principal
    {
        views: uint,
        unique-users: uint,
        peak-earnings: uint,
        last-updated: uint
    }
)

(define-public (update-analytics (work-owner principal))
    (let (
        (current-stats (default-to 
            {views: u0, unique-users: u0, peak-earnings: u0, last-updated: u0} 
            (map-get? work-analytics work-owner)))
    )
        (ok (map-set work-analytics work-owner
            (merge current-stats {
                views: (+ (get views current-stats) u1),
                last-updated: stacks-block-height
            })))
    )
)

(define-read-only (get-work-analytics (owner principal))
    (ok (map-get? work-analytics owner))
)


(define-map promotional-discounts
    { owner: principal, promo-id: uint }
    {
        start-height: uint,
        end-height: uint,
        discount-percentage: uint,
        active: bool
    }
)

(define-data-var promo-counter uint u0)

(define-public (create-promotion (duration uint) (discount uint))
    (let
        ((work (unwrap! (map-get? creative-works tx-sender) err-not-found))
         (promo-id (+ (var-get promo-counter) u1)))
        (asserts! (<= discount u1000) err-invalid-percentage)
        (var-set promo-counter promo-id)
        (ok (map-set promotional-discounts
            { owner: tx-sender, promo-id: promo-id }
            {
                start-height: stacks-block-height,
                end-height: (+ stacks-block-height duration),
                discount-percentage: discount,
                active: true
            }
        ))
    )
)

(define-read-only (get-discounted-price (owner principal) (original-price uint))
    (let
        ((promo (map-get? promotional-discounts { owner: owner, promo-id: (var-get promo-counter) })))
        (if (and 
            (is-some promo)
            (active-promotion? (unwrap-panic promo)))
            (let ((discount (get discount-percentage (unwrap-panic promo))))
                (ok (- original-price (/ (* original-price discount) u1000))))
            (ok original-price)
        )
    )
)

(define-private (active-promotion? (promo {start-height: uint, end-height: uint, discount-percentage: uint, active: bool}))
    (and
        (get active promo)
        (>= (get end-height promo) stacks-block-height)
        (<= (get start-height promo) stacks-block-height)
    )
)


(define-map work-bundles
    (string-ascii 50)
    {
        creator: principal,
        works: (list 10 principal),
        bundle-price: uint,
        active: bool
    }
)

(define-public (create-bundle (bundle-id (string-ascii 50)) (works (list 10 principal)) (price uint))
    (let
        ((work-exists (map-get? work-bundles bundle-id)))
        (asserts! (is-none work-exists) err-already-registered)
        (ok (map-set work-bundles bundle-id
            {
                creator: tx-sender,
                works: works,
                bundle-price: price,
                active: true
            }
        ))
    )
)

(define-public (purchase-bundle (bundle-id (string-ascii 50)))
    (let
        ((bundle (unwrap! (map-get? work-bundles bundle-id) err-not-found)))
        (asserts! (get active bundle) err-unauthorized)
        (try! (stx-transfer? (get bundle-price bundle) tx-sender (get creator bundle)))
        (map register-bundle-access (get works bundle))
        (ok true)
    )
)

(define-private (register-bundle-access (work-owner principal))
    (map-set usage-tracking
        {work-owner: work-owner, user: tx-sender}
        {
            last-payment: stacks-block-height,
            total-paid: u0,
            license-expiry: (+ stacks-block-height u14400)
        }
    )
)


(define-map escrow-accounts
    { depositor: principal, work-owner: principal }
    {
        balance: uint,
        auto-pay-amount: uint,
        auto-pay-interval: uint,
        last-auto-pay: uint,
        active: bool
    }
)

(define-map escrow-transactions
    { depositor: principal, work-owner: principal, tx-id: uint }
    {
        amount: uint,
        transaction-type: (string-ascii 10),
        timestamp: uint
    }
)

(define-data-var escrow-tx-counter uint u0)

(define-public (create-escrow-account (work-owner principal) (initial-deposit uint) (auto-amount uint) (interval uint))
    (let
        ((work (unwrap! (map-get? creative-works work-owner) err-not-found))
         (tx-id (+ (var-get escrow-tx-counter) u1)))
        (try! (stx-transfer? initial-deposit tx-sender (as-contract tx-sender)))
        (var-set escrow-tx-counter tx-id)
        (map-set escrow-accounts
            { depositor: tx-sender, work-owner: work-owner }
            {
                balance: initial-deposit,
                auto-pay-amount: auto-amount,
                auto-pay-interval: interval,
                last-auto-pay: stacks-block-height,
                active: true
            }
        )
        (map-set escrow-transactions
            { depositor: tx-sender, work-owner: work-owner, tx-id: tx-id }
            {
                amount: initial-deposit,
                transaction-type: "deposit",
                timestamp: stacks-block-height
            }
        )
        (ok tx-id)
    )
)

(define-public (deposit-to-escrow (work-owner principal) (amount uint))
    (let
        ((account (unwrap! (map-get? escrow-accounts { depositor: tx-sender, work-owner: work-owner }) err-not-found))
         (tx-id (+ (var-get escrow-tx-counter) u1)))
        (asserts! (get active account) err-unauthorized)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set escrow-tx-counter tx-id)
        (map-set escrow-accounts
            { depositor: tx-sender, work-owner: work-owner }
            (merge account { balance: (+ (get balance account) amount) })
        )
        (map-set escrow-transactions
            { depositor: tx-sender, work-owner: work-owner, tx-id: tx-id }
            {
                amount: amount,
                transaction-type: "deposit",
                timestamp: stacks-block-height
            }
        )
        (ok tx-id)
    )
)

(define-public (process-auto-payment (depositor principal) (work-owner principal))
    (let
        ((account (unwrap! (map-get? escrow-accounts { depositor: depositor, work-owner: work-owner }) err-not-found))
         (work (unwrap! (map-get? creative-works work-owner) err-not-found))
         (auto-amount (get auto-pay-amount account))
         (current-balance (get balance account))
         (platform-cut (/ (* auto-amount (var-get platform-fee)) u1000))
         (creator-amount (- auto-amount platform-cut))
         (tx-id (+ (var-get escrow-tx-counter) u1)))
        (asserts! (get active account) err-unauthorized)
        (asserts! (>= current-balance auto-amount) err-not-found)
        (asserts! (>= stacks-block-height (+ (get last-auto-pay account) (get auto-pay-interval account))) err-unauthorized)
        
        (try! (as-contract (stx-transfer? platform-cut tx-sender contract-owner)))
        (try! (as-contract (stx-transfer? creator-amount tx-sender work-owner)))
        
        (var-set escrow-tx-counter tx-id)
        (map-set escrow-accounts
            { depositor: depositor, work-owner: work-owner }
            (merge account {
                balance: (- current-balance auto-amount),
                last-auto-pay: stacks-block-height
            })
        )
        
        (map-set escrow-transactions
            { depositor: depositor, work-owner: work-owner, tx-id: tx-id }
            {
                amount: auto-amount,
                transaction-type: "payment",
                timestamp: stacks-block-height
            }
        )
        
        (map-set usage-tracking { work-owner: work-owner, user: depositor }
            {
                last-payment: stacks-block-height,
                total-paid: (+ auto-amount (default-to u0 (get total-paid (map-get? usage-tracking { work-owner: work-owner, user: depositor })))),
                license-expiry: (+ stacks-block-height u1440)
            }
        )
        
        (map-set creative-works work-owner
            (merge work { total-earnings: (+ (get total-earnings work) auto-amount) })
        )
        
        (ok tx-id)
    )
)

(define-public (withdraw-from-escrow (work-owner principal) (amount uint))
    (let
        ((account (unwrap! (map-get? escrow-accounts { depositor: tx-sender, work-owner: work-owner }) err-not-found))
         (tx-id (+ (var-get escrow-tx-counter) u1)))
        (asserts! (get active account) err-unauthorized)
        (asserts! (>= (get balance account) amount) err-not-found)
        
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        (var-set escrow-tx-counter tx-id)
        (map-set escrow-accounts
            { depositor: tx-sender, work-owner: work-owner }
            (merge account { balance: (- (get balance account) amount) })
        )
        
        (map-set escrow-transactions
            { depositor: tx-sender, work-owner: work-owner, tx-id: tx-id }
            {
                amount: amount,
                transaction-type: "withdraw",
                timestamp: stacks-block-height
            }
        )
        (ok tx-id)
    )
)

(define-public (update-auto-payment-settings (work-owner principal) (new-amount uint) (new-interval uint))
    (let
        ((account (unwrap! (map-get? escrow-accounts { depositor: tx-sender, work-owner: work-owner }) err-not-found)))
        (asserts! (get active account) err-unauthorized)
        (ok (map-set escrow-accounts
            { depositor: tx-sender, work-owner: work-owner }
            (merge account {
                auto-pay-amount: new-amount,
                auto-pay-interval: new-interval
            })
        ))
    )
)

(define-public (deactivate-escrow-account (work-owner principal))
    (let
        ((account (unwrap! (map-get? escrow-accounts { depositor: tx-sender, work-owner: work-owner }) err-not-found)))
        (ok (map-set escrow-accounts
            { depositor: tx-sender, work-owner: work-owner }
            (merge account { active: false })
        ))
    )
)

(define-constant err-dispute-not-found (err u105))
(define-constant err-dispute-already-resolved (err u106))
(define-constant err-invalid-dispute-status (err u107))
(define-constant err-dispute-already-exists (err u108))

(define-data-var dispute-counter uint u0)

(define-map work-disputes
    uint
    {
        dispute-id: uint,
        work-owner: principal,
        complainant: principal,
        dispute-type: (string-ascii 20),
        description: (string-ascii 300),
        status: (string-ascii 15),
        created-at: uint,
        resolved-at: uint,
        resolution: (string-ascii 200),
        evidence-hash: (string-ascii 64)
    }
)

;; Dynamic Royalty Rate Adjustment System

;; Data maps for dynamic pricing
(define-map dynamic-pricing-rules
    principal
    {
        enabled: bool,
        base-rate: uint,
        min-rate: uint,
        max-rate: uint,
        adjustment-factor: uint,
        peak-hours-start: uint,
        peak-hours-end: uint,
        peak-multiplier: uint
    }
)

(define-map usage-metrics
    principal
    {
        daily-usage-count: uint,
        weekly-usage-count: uint,
        last-usage-day: uint,
        last-usage-week: uint,
        popularity-score: uint,
        current-adjusted-rate: uint
    }
)

(define-map rate-history
    {work-owner: principal, timestamp: uint}
    {
        old-rate: uint,
        new-rate: uint,
        adjustment-reason: (string-ascii 30),
        usage-count: uint
    }
)

;; Configure dynamic pricing rules for a work
(define-public (configure-dynamic-pricing 
    (base-rate uint) 
    (min-rate uint) 
    (max-rate uint) 
    (adjustment-factor uint)
    (peak-start uint)
    (peak-end uint)
    (peak-multiplier uint))
    (let ((work (unwrap! (map-get? creative-works tx-sender) err-not-found)))
        (asserts! (<= min-rate base-rate) err-rate-below-minimum)
        (asserts! (<= base-rate max-rate) err-rate-above-maximum)
        (asserts! (<= adjustment-factor u500) err-invalid-adjustment-rule) ;; Max 50% adjustment
        (asserts! (< peak-start u24) err-invalid-adjustment-rule) ;; Valid hour
        (asserts! (< peak-end u24) err-invalid-adjustment-rule) ;; Valid hour
        (asserts! (<= peak-multiplier u300) err-invalid-adjustment-rule) ;; Max 3x multiplier
        
        (map-set dynamic-pricing-rules tx-sender {
            enabled: true,
            base-rate: base-rate,
            min-rate: min-rate,
            max-rate: max-rate,
            adjustment-factor: adjustment-factor,
            peak-hours-start: peak-start,
            peak-hours-end: peak-end,
            peak-multiplier: peak-multiplier
        })
        
        ;; Initialize usage metrics
        (map-set usage-metrics tx-sender {
            daily-usage-count: u0,
            weekly-usage-count: u0,
            last-usage-day: (get-current-day),
            last-usage-week: (get-current-week),
            popularity-score: u100, ;; Start with base score
            current-adjusted-rate: base-rate
        })
        
        (ok true)
    )
)

;; Calculate adjusted rate based on current metrics
(define-public (calculate-adjusted-rate (work-owner principal))
    (let (
        (pricing-rules (unwrap! (map-get? dynamic-pricing-rules work-owner) err-not-found))
        (metrics (unwrap! (map-get? usage-metrics work-owner) err-not-found))
        (current-hour (mod stacks-block-height u144)) ;; Rough hour calculation
        (current-day (get-current-day))
        (current-week (get-current-week))
    )
        (asserts! (get enabled pricing-rules) err-unauthorized)
        
        ;; Reset daily/weekly counters if needed
        (let (
            (updated-metrics (update-usage-counters metrics current-day current-week))
            (base-rate (get base-rate pricing-rules))
            (popularity-adjustment (calculate-popularity-adjustment 
                (get popularity-score updated-metrics) 
                (get adjustment-factor pricing-rules)))
            (time-adjustment (calculate-time-adjustment 
                current-hour 
                (get peak-hours-start pricing-rules)
                (get peak-hours-end pricing-rules)
                (get peak-multiplier pricing-rules)))
        )
            (let (
                (adjusted-rate (apply-rate-adjustments 
                    base-rate 
                    popularity-adjustment 
                    time-adjustment))
                (final-rate (enforce-rate-limits 
                    adjusted-rate 
                    (get min-rate pricing-rules) 
                    (get max-rate pricing-rules)))
            )
                ;; Update current rate in metrics
                (map-set usage-metrics work-owner
                    (merge updated-metrics {current-adjusted-rate: final-rate}))
                
                (ok final-rate)
            )
        )
    )
)

;; Update usage metrics when payment occurs
(define-public (track-usage-and-update-rate (work-owner principal))
    (let (
        (metrics (unwrap! (map-get? usage-metrics work-owner) err-not-found))
        (current-day (get-current-day))
        (current-week (get-current-week))
        (updated-daily (+ (get daily-usage-count metrics) u1))
        (updated-weekly (+ (get weekly-usage-count metrics) u1))
    )
        ;; Update usage counts
        (map-set usage-metrics work-owner
            (merge metrics {
                daily-usage-count: updated-daily,
                weekly-usage-count: updated-weekly,
                last-usage-day: current-day,
                last-usage-week: current-week,
                popularity-score: (calculate-new-popularity-score 
                    (get popularity-score metrics) 
                    updated-daily 
                    updated-weekly)
            }))
        
        ;; Recalculate adjusted rate
        (calculate-adjusted-rate work-owner)
    )
)

;; Enhanced pay-royalty with dynamic pricing
(define-public (pay-dynamic-royalty (work-owner principal))
    (let (
        (work (unwrap! (map-get? creative-works work-owner) err-not-found))
        (pricing-rules (map-get? dynamic-pricing-rules work-owner))
    )
        (if (is-some pricing-rules)
            ;; Use dynamic pricing
            (let (
                (adjusted-rate (unwrap! (calculate-adjusted-rate work-owner) err-not-found))
                (old-rate (get current-adjusted-rate 
                    (unwrap! (map-get? usage-metrics work-owner) err-not-found)))
            )
                ;; Record rate change if significant
                (if (> (abs-diff adjusted-rate old-rate) u10) ;; 1% change threshold
                    (map-set rate-history 
                        {work-owner: work-owner, timestamp: stacks-block-height}
                        {
                            old-rate: old-rate,
                            new-rate: adjusted-rate,
                            adjustment-reason: "usage-based",
                            usage-count: (get daily-usage-count 
                                (unwrap! (map-get? usage-metrics work-owner) err-not-found))
                        })
                    true)
                
                ;; Update usage and pay
                (try! (track-usage-and-update-rate work-owner))
                (pay-royalty work-owner adjusted-rate)
            )
            ;; Fall back to regular pricing
            (pay-royalty work-owner (get royalty-percentage work))
        )
    )
)

;; Disable dynamic pricing
(define-public (disable-dynamic-pricing)
    (let ((pricing-rules (unwrap! (map-get? dynamic-pricing-rules tx-sender) err-not-found)))
        (ok (map-set dynamic-pricing-rules tx-sender
            (merge pricing-rules {enabled: false})))
    )
)

;; Read-only functions for dynamic pricing

(define-read-only (get-dynamic-pricing-rules (work-owner principal))
    (ok (map-get? dynamic-pricing-rules work-owner))
)

(define-read-only (get-usage-metrics (work-owner principal))
    (ok (map-get? usage-metrics work-owner))
)

(define-read-only (get-rate-history (work-owner principal) (timestamp uint))
    (ok (map-get? rate-history {work-owner: work-owner, timestamp: timestamp}))
)

(define-read-only (get-current-adjusted-rate (work-owner principal))
    (let ((metrics (map-get? usage-metrics work-owner)))
        (if (is-some metrics)
            (ok (some (get current-adjusted-rate (unwrap-panic metrics))))
            (ok none)
        )
    )
)

;; Private helper functions

(define-private (get-current-day)
    (/ stacks-block-height u144) ;; Approximately 1 day in blocks
)

(define-private (get-current-week)
    (/ stacks-block-height u1008) ;; Approximately 1 week in blocks
)

(define-private (update-usage-counters (metrics {daily-usage-count: uint, weekly-usage-count: uint, last-usage-day: uint, last-usage-week: uint, popularity-score: uint, current-adjusted-rate: uint}) (current-day uint) (current-week uint))
    (let (
        (daily-count (if (> current-day (get last-usage-day metrics)) u0 (get daily-usage-count metrics)))
        (weekly-count (if (> current-week (get last-usage-week metrics)) u0 (get weekly-usage-count metrics)))
    )
        (merge metrics {
            daily-usage-count: daily-count,
            weekly-usage-count: weekly-count
        })
    )
)

(define-private (calculate-popularity-adjustment (popularity-score uint) (adjustment-factor uint))
    (if (> popularity-score u150) ;; High popularity
        adjustment-factor ;; Increase rate
        (if (< popularity-score u50) ;; Low popularity
            (- u0 adjustment-factor) ;; Decrease rate
            u0 ;; No change
        )
    )
)

(define-private (calculate-time-adjustment (current-hour uint) (peak-start uint) (peak-end uint) (peak-multiplier uint))
    (if (and (>= current-hour peak-start) (<= current-hour peak-end))
        peak-multiplier ;; Peak hours multiplier
        u100 ;; Regular hours (100% = no change)
    )
)

(define-private (apply-rate-adjustments (base-rate uint) (popularity-adj uint) (time-adj uint))
    (let (
        (popularity-adjusted (+ base-rate (/ (* base-rate popularity-adj) u1000)))
        (time-adjusted (/ (* popularity-adjusted time-adj) u100))
    )
        time-adjusted
    )
)

(define-private (enforce-rate-limits (rate uint) (min-rate uint) (max-rate uint))
    (if (< rate min-rate)
        min-rate
        (if (> rate max-rate)
            max-rate
            rate
        )
    )
)

(define-private (calculate-new-popularity-score (current-score uint) (daily-usage uint) (weekly-usage uint))
    (let (
        (daily-factor (if (<= daily-usage u50) daily-usage u50)) ;; Cap daily influence
        (weekly-factor (if (<= (/ weekly-usage u7) u20) (/ weekly-usage u7) u20)) ;; Average weekly influence
        (new-score (+ current-score daily-factor weekly-factor))
        (clamped-score (if (< new-score u10) u10 (if (> new-score u300) u300 new-score)))
    )
        clamped-score ;; Keep score between 10-300
    )
)

(define-private (abs-diff (a uint) (b uint))
    (if (>= a b) (- a b) (- b a))
)

(define-map dispute-votes
    {dispute-id: uint, voter: principal}
    {
        vote: (string-ascii 10),
        timestamp: uint
    }
)

(define-map dispute-evidence
    {dispute-id: uint, evidence-id: uint}
    {
        submitter: principal,
        evidence-type: (string-ascii 20),
        evidence-hash: (string-ascii 64),
        timestamp: uint
    }
)

(define-public (file-dispute (work-owner principal) (dispute-type (string-ascii 20)) (description (string-ascii 300)) (evidence-hash (string-ascii 64)))
    (let
        ((work (unwrap! (map-get? creative-works work-owner) err-not-found))
         (dispute-id (+ (var-get dispute-counter) u1)))
        (asserts! (not (is-eq tx-sender work-owner)) err-unauthorized)
        (var-set dispute-counter dispute-id)
        (map-set work-disputes dispute-id
            {
                dispute-id: dispute-id,
                work-owner: work-owner,
                complainant: tx-sender,
                dispute-type: dispute-type,
                description: description,
                status: "pending",
                created-at: stacks-block-height,
                resolved-at: u0,
                resolution: "",
                evidence-hash: evidence-hash
            }
        )
        (ok dispute-id)
    )
)

(define-public (submit-dispute-evidence (dispute-id uint) (evidence-type (string-ascii 20)) (evidence-hash (string-ascii 64)))
    (let
        ((dispute (unwrap! (map-get? work-disputes dispute-id) err-dispute-not-found))
         (evidence-id (+ dispute-id u1)))
        (asserts! (is-eq (get status dispute) "pending") err-dispute-already-resolved)
        (asserts! (or (is-eq tx-sender (get work-owner dispute)) (is-eq tx-sender (get complainant dispute))) err-unauthorized)
        (map-set dispute-evidence
            {dispute-id: dispute-id, evidence-id: evidence-id}
            {
                submitter: tx-sender,
                evidence-type: evidence-type,
                evidence-hash: evidence-hash,
                timestamp: stacks-block-height
            }
        )
        (ok evidence-id)
    )
)

(define-public (vote-on-dispute (dispute-id uint) (vote (string-ascii 10)))
    (let
        ((dispute (unwrap! (map-get? work-disputes dispute-id) err-dispute-not-found)))
        (asserts! (is-eq (get status dispute) "pending") err-dispute-already-resolved)
        (asserts! (not (is-eq tx-sender (get work-owner dispute))) err-unauthorized)
        (asserts! (not (is-eq tx-sender (get complainant dispute))) err-unauthorized)
        (map-set dispute-votes
            {dispute-id: dispute-id, voter: tx-sender}
            {
                vote: vote,
                timestamp: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (resolve-dispute (dispute-id uint) (resolution (string-ascii 200)) (final-status (string-ascii 15)))
    (let
        ((dispute (unwrap! (map-get? work-disputes dispute-id) err-dispute-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-eq (get status dispute) "pending") err-dispute-already-resolved)
        (map-set work-disputes dispute-id
            (merge dispute {
                status: final-status,
                resolved-at: stacks-block-height,
                resolution: resolution
            })
        )
        (ok true)
    )
)

(define-public (escalate-dispute (dispute-id uint))
    (let
        ((dispute (unwrap! (map-get? work-disputes dispute-id) err-dispute-not-found)))
        (asserts! (is-eq (get status dispute) "pending") err-dispute-already-resolved)
        (asserts! (or (is-eq tx-sender (get work-owner dispute)) (is-eq tx-sender (get complainant dispute))) err-unauthorized)
        (map-set work-disputes dispute-id
            (merge dispute {status: "escalated"})
        )
        (ok true)
    )
)

(define-read-only (get-dispute-details (dispute-id uint))
    (ok (map-get? work-disputes dispute-id))
)

(define-read-only (get-dispute-vote (dispute-id uint) (voter principal))
    (ok (map-get? dispute-votes {dispute-id: dispute-id, voter: voter}))
)

(define-read-only (get-dispute-evidence (dispute-id uint) (evidence-id uint))
    (ok (map-get? dispute-evidence {dispute-id: dispute-id, evidence-id: evidence-id}))
)

(define-read-only (get-total-disputes)
    (ok (var-get dispute-counter))
)

(define-read-only (check-dispute-status (dispute-id uint))
    (let
        ((dispute (map-get? work-disputes dispute-id)))
        (if (is-some dispute)
            (ok (get status (unwrap-panic dispute)))
            (ok "not-found")
        )
    )
)

(define-read-only (get-escrow-account (depositor principal) (work-owner principal))
    (ok (map-get? escrow-accounts { depositor: depositor, work-owner: work-owner }))
)

(define-read-only (get-escrow-transaction (depositor principal) (work-owner principal) (tx-id uint))
    (ok (map-get? escrow-transactions { depositor: depositor, work-owner: work-owner, tx-id: tx-id }))
)

(define-read-only (check-auto-payment-due (depositor principal) (work-owner principal))
    (let
        ((account (map-get? escrow-accounts { depositor: depositor, work-owner: work-owner })))
        (if (is-some account)
            (let ((acc (unwrap-panic account)))
                (ok (and
                    (get active acc)
                    (>= stacks-block-height (+ (get last-auto-pay acc) (get auto-pay-interval acc)))
                    (>= (get balance acc) (get auto-pay-amount acc))
                ))
            )
            (ok false)
        )
    )
)



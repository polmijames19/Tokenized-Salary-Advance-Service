;; AdvancePredictor - Intelligent advance forecasting and cash flow optimization
;; Predicts future advance needs based on employee patterns and seasonal trends

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u600))
(define-constant err-invalid-parameters (err u601))
(define-constant err-prediction-not-found (err u602))
(define-constant err-insufficient-data (err u603))
(define-constant err-employee-not-found (err u604))

;; Data variables
(define-data-var next-prediction-id uint u1)
(define-data-var total-predictions uint u0)
(define-data-var prediction-accuracy-rate uint u0)

;; Employee advance patterns
(define-map employee-advance-patterns
    principal
    {
        average-advance-amount: uint,
        advance-frequency: uint,
        seasonal-factor: uint,
        last-advance-date: uint,
        pattern-confidence: uint,
        predicted-next-advance: uint,
        trend-direction: (string-ascii 15),
        last-updated: uint
    }
)

;; Seasonal advance trends
(define-map seasonal-trends
    uint ;; Month (1-12)
    {
        month-number: uint,
        advance-multiplier: uint,
        typical-increase: uint,
        confidence-level: uint,
        historical-data-points: uint,
        last-calculated: uint
    }
)

;; Cash flow forecasts
(define-map cash-flow-forecasts
    uint
    {
        forecast-period: uint,
        predicted-total-advances: uint,
        predicted-total-amount: uint,
        recommended-reserve: uint,
        confidence-score: uint,
        risk-level: (string-ascii 10),
        generated-at: uint,
        forecast-horizon: uint
    }
)

;; Individual employee predictions
(define-map employee-predictions
    {employee: principal, prediction-id: uint}
    {
        predicted-amount: uint,
        predicted-date: uint,
        probability: uint,
        reasoning: (string-ascii 100),
        based-on-pattern: bool,
        seasonal-adjustment: uint,
        confidence-level: uint
    }
)

;; Advance request patterns by time period
(define-map advance-timing-patterns
    uint ;; Day of month (1-31)
    {
        day-of-month: uint,
        request-frequency: uint,
        average-amount: uint,
        total-requests: uint,
        pattern-strength: uint
    }
)

;; Helper functions

;; Calculate current month approximation
(define-private (calculate-current-month)
    (+ (mod (/ stacks-block-height u4320) u12) u1) ;; Rough monthly calculation
)

;; Determine trend direction
(define-private (determine-trend-direction (previous-amount uint) (current-amount uint))
    (if (> current-amount previous-amount) "increasing"
        (if (< current-amount previous-amount) "decreasing" "stable"))
)

;; Calculate pattern confidence
(define-private (calculate-pattern-confidence (frequency uint) (average-amount uint))
    (if (and (> frequency u0) (> average-amount u0))
        (+ u40 (/ frequency u100) (/ average-amount u10000))
        u30)
)

;; Calculate advance probability
(define-private (calculate-advance-probability (pattern-data (tuple (average-advance-amount uint) (advance-frequency uint) (seasonal-factor uint) (last-advance-date uint) (pattern-confidence uint) (predicted-next-advance uint) (trend-direction (string-ascii 15)) (last-updated uint))) (seasonal-data (tuple (month-number uint) (advance-multiplier uint) (typical-increase uint) (confidence-level uint) (historical-data-points uint) (last-calculated uint))))
    (let
        (
            (pattern-strength (get pattern-confidence pattern-data))
            (seasonal-strength (get confidence-level seasonal-data))
            (frequency-factor (if (> (get advance-frequency pattern-data) u0) u80 u40))
        )
        (/ (+ pattern-strength seasonal-strength frequency-factor) u3)
    )
)

;; Generate prediction reasoning
(define-private (generate-prediction-reasoning (pattern-data (tuple (average-advance-amount uint) (advance-frequency uint) (seasonal-factor uint) (last-advance-date uint) (pattern-confidence uint) (predicted-next-advance uint) (trend-direction (string-ascii 15)) (last-updated uint))) (seasonal-data (tuple (month-number uint) (advance-multiplier uint) (typical-increase uint) (confidence-level uint) (historical-data-points uint) (last-calculated uint))))
    (if (> (get advance-multiplier seasonal-data) u110)
        "High seasonal demand expected based on historical patterns"
        (if (is-eq (get trend-direction pattern-data) "increasing")
            "Increasing advance pattern detected in employee behavior"
            "Stable advance pattern with normal seasonal adjustment"
        )
    )
)

;; Get seasonal factor for month
(define-private (get-seasonal-factor (month uint))
    (let
        (
            (seasonal-data (map-get? seasonal-trends month))
        )
        (if (is-some seasonal-data)
            (get advance-multiplier (unwrap-panic seasonal-data))
            u100
        )
    )
)

;; Estimate total advances for period
(define-private (estimate-total-advances (horizon uint) (seasonal-factor uint))
    ;; Simplified estimation - would need more data in real implementation
    (/ (* horizon seasonal-factor) u100)
)

;; Estimate total amount for period
(define-private (estimate-total-amount (horizon uint) (seasonal-factor uint))
    ;; Simplified estimation based on average patterns
    (/ (* (* horizon u50000) seasonal-factor) u100) ;; Base estimate: 50k per period
)

;; Assess cash flow risk
(define-private (assess-cash-flow-risk (estimated-amount uint) (recommended-reserve uint))
    (if (> estimated-amount u100000) "high"
        (if (> estimated-amount u50000) "medium" "low"))
)

;; Calculate amount prediction accuracy
(define-private (calculate-amount-accuracy (predicted uint) (actual uint))
    (if (> predicted u0)
        (let
            (
                (difference (if (> actual predicted) (- actual predicted) (- predicted actual)))
                (percentage-diff (/ (* difference u100) predicted))
            )
            (if (<= percentage-diff u10) u100 ;; Within 10%
                (if (<= percentage-diff u25) u75  ;; Within 25%
                    u50)) ;; More than 25% off
        )
        u0
    )
)

;; Calculate date prediction accuracy
(define-private (calculate-date-accuracy (predicted-date uint) (actual-date uint))
    (let
        (
            (date-diff (if (> actual-date predicted-date) 
                         (- actual-date predicted-date) 
                         (- predicted-date actual-date)))
        )
        (if (<= date-diff u144) u100      ;; Within 1 day
            (if (<= date-diff u720) u80   ;; Within 5 days
                (if (<= date-diff u1440) u60 u40))) ;; Within 10 days or more
    )
)

;; Public functions

;; Initialize seasonal trends (admin only)
(define-public (init-seasonal-trends)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        ;; Initialize basic seasonal multipliers
        (map-set seasonal-trends u1  ;; January
            {month-number: u1, advance-multiplier: u120, typical-increase: u20, 
             confidence-level: u75, historical-data-points: u0, last-calculated: stacks-block-height})
        (map-set seasonal-trends u12 ;; December
            {month-number: u12, advance-multiplier: u150, typical-increase: u50,
             confidence-level: u80, historical-data-points: u0, last-calculated: stacks-block-height})
        (map-set seasonal-trends u6  ;; June (summer)
            {month-number: u6, advance-multiplier: u110, typical-increase: u10,
             confidence-level: u70, historical-data-points: u0, last-calculated: stacks-block-height})
        (ok true)
    )
)

;; Record advance pattern for employee
(define-public (record-advance-pattern
    (employee principal)
    (advance-amount uint)
    (advance-date uint))
    (let
        (
            (current-pattern (default-to 
                {average-advance-amount: u0, advance-frequency: u0, seasonal-factor: u100,
                 last-advance-date: u0, pattern-confidence: u50, predicted-next-advance: u0,
                 trend-direction: "unknown", last-updated: u0}
                (map-get? employee-advance-patterns employee)))
            (time-since-last (if (> (get last-advance-date current-pattern) u0)
                               (- advance-date (get last-advance-date current-pattern))
                               u0))
            (new-frequency (if (> time-since-last u0)
                             (/ (+ (get advance-frequency current-pattern) time-since-last) u2)
                             (get advance-frequency current-pattern)))
            (new-average (/ (+ (get average-advance-amount current-pattern) advance-amount) u2))
            (trend (determine-trend-direction (get average-advance-amount current-pattern) advance-amount))
        )
        (map-set employee-advance-patterns employee
            {
                average-advance-amount: new-average,
                advance-frequency: new-frequency,
                seasonal-factor: u100, ;; Default, can be enhanced
                last-advance-date: advance-date,
                pattern-confidence: (calculate-pattern-confidence new-frequency new-average),
                predicted-next-advance: (+ advance-date new-frequency),
                trend-direction: trend,
                last-updated: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Generate advance prediction for employee
(define-public (generate-employee-prediction (employee principal))
    (let
        (
            (prediction-id (var-get next-prediction-id))
            (pattern-data (unwrap! (map-get? employee-advance-patterns employee) err-employee-not-found))
            (current-month (calculate-current-month))
            (seasonal-data (default-to 
                {month-number: current-month, advance-multiplier: u100, typical-increase: u0,
                 confidence-level: u50, historical-data-points: u0, last-calculated: u0}
                (map-get? seasonal-trends current-month)))
            (base-amount (get average-advance-amount pattern-data))
            (seasonal-multiplier (get advance-multiplier seasonal-data))
            (predicted-amount (/ (* base-amount seasonal-multiplier) u100))
            (predicted-date (get predicted-next-advance pattern-data))
            (probability (calculate-advance-probability pattern-data seasonal-data))
            (confidence (/ (+ (get pattern-confidence pattern-data) (get confidence-level seasonal-data)) u2))
        )
        (asserts! (> (get pattern-confidence pattern-data) u30) err-insufficient-data)
        
        (map-set employee-predictions {employee: employee, prediction-id: prediction-id}
            {
                predicted-amount: predicted-amount,
                predicted-date: predicted-date,
                probability: probability,
                reasoning: (generate-prediction-reasoning pattern-data seasonal-data),
                based-on-pattern: true,
                seasonal-adjustment: (get typical-increase seasonal-data),
                confidence-level: confidence
            }
        )
        
        (var-set next-prediction-id (+ prediction-id u1))
        (var-set total-predictions (+ (var-get total-predictions) u1))
        (ok prediction-id)
    )
)

;; Generate cash flow forecast
(define-public (generate-cash-flow-forecast (forecast-horizon uint))
    (let
        (
            (forecast-id (var-get next-prediction-id))
            (current-month (calculate-current-month))
            (seasonal-factor (get-seasonal-factor current-month))
            (estimated-total-advances (estimate-total-advances forecast-horizon seasonal-factor))
            (estimated-amount (estimate-total-amount forecast-horizon seasonal-factor))
            (recommended-reserve (/ (* estimated-amount u110) u100)) ;; 10% buffer
            (risk-assessment (assess-cash-flow-risk estimated-amount recommended-reserve))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (and (> forecast-horizon u0) (<= forecast-horizon u12)) err-invalid-parameters)
        
        (map-set cash-flow-forecasts forecast-id
            {
                forecast-period: forecast-horizon,
                predicted-total-advances: estimated-total-advances,
                predicted-total-amount: estimated-amount,
                recommended-reserve: recommended-reserve,
                confidence-score: u75, ;; Base confidence
                risk-level: risk-assessment,
                generated-at: stacks-block-height,
                forecast-horizon: forecast-horizon
            }
        )
        
        (var-set next-prediction-id (+ forecast-id u1))
        (ok forecast-id)
    )
)

;; Update seasonal trend data
(define-public (update-seasonal-trend
    (month uint)
    (multiplier uint)
    (typical-increase uint))
    (let
        (
            (trend-data (default-to 
                {month-number: month, advance-multiplier: u100, typical-increase: u0,
                 confidence-level: u50, historical-data-points: u0, last-calculated: u0}
                (map-get? seasonal-trends month)))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (and (>= month u1) (<= month u12)) err-invalid-parameters)
        (asserts! (and (>= multiplier u50) (<= multiplier u300)) err-invalid-parameters)
        
        (map-set seasonal-trends month
            (merge trend-data {
                advance-multiplier: multiplier,
                typical-increase: typical-increase,
                historical-data-points: (+ (get historical-data-points trend-data) u1),
                last-calculated: stacks-block-height
            })
        )
        (ok true)
    )
)

;; Read-only functions

;; Get employee advance pattern
(define-read-only (get-employee-advance-pattern (employee principal))
    (map-get? employee-advance-patterns employee)
)

;; Get seasonal trend data
(define-read-only (get-seasonal-trend (month uint))
    (map-get? seasonal-trends month)
)

;; Get cash flow forecast
(define-read-only (get-cash-flow-forecast (forecast-id uint))
    (map-get? cash-flow-forecasts forecast-id)
)

;; Get employee prediction
(define-read-only (get-employee-prediction (employee principal) (prediction-id uint))
    (map-get? employee-predictions {employee: employee, prediction-id: prediction-id})
)

;; Get prediction accuracy stats
(define-read-only (get-prediction-stats)
    (ok {
        total-predictions: (var-get total-predictions),
        accuracy-rate: (var-get prediction-accuracy-rate),
        next-prediction-id: (var-get next-prediction-id)
    })
)

;; Calculate recommended cash reserve
(define-read-only (calculate-recommended-reserve (authority principal))
    (let
        (
            (current-month (calculate-current-month))
            (seasonal-factor (get-seasonal-factor current-month))
            (base-reserve u100000) ;; Base reserve amount
            (seasonal-reserve (/ (* base-reserve seasonal-factor) u100))
        )
        (ok {
            base-reserve: base-reserve,
            seasonal-adjustment: seasonal-factor,
            recommended-total: seasonal-reserve,
            risk-buffer: (/ seasonal-reserve u10)
        })
    )
)

;; Validate prediction accuracy (admin only)
(define-public (validate-prediction-accuracy
    (employee principal)
    (prediction-id uint)
    (actual-amount uint)
    (actual-date uint))
    (let
        (
            (prediction (unwrap! (map-get? employee-predictions {employee: employee, prediction-id: prediction-id}) err-prediction-not-found))
            (predicted-amount (get predicted-amount prediction))
            (predicted-date (get predicted-date prediction))
            (amount-accuracy (calculate-amount-accuracy predicted-amount actual-amount))
            (date-accuracy (calculate-date-accuracy predicted-date actual-date))
            (overall-accuracy (/ (+ amount-accuracy date-accuracy) u2))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        
        ;; Update system accuracy rate
        (let
            (
                (current-accuracy (var-get prediction-accuracy-rate))
                (total-preds (var-get total-predictions))
                (new-accuracy (if (> total-preds u0)
                                (/ (+ (* current-accuracy total-preds) overall-accuracy) (+ total-preds u1))
                                overall-accuracy))
            )
            (var-set prediction-accuracy-rate new-accuracy)
        )
        (ok overall-accuracy)
    )
)

;; Administrative functions

;; Update pattern confidence manually (admin only)
(define-public (update-pattern-confidence
    (employee principal)
    (new-confidence uint))
    (let
        (
            (pattern-data (unwrap! (map-get? employee-advance-patterns employee) err-employee-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (<= new-confidence u100) err-invalid-parameters)
        
        (map-set employee-advance-patterns employee
            (merge pattern-data {
                pattern-confidence: new-confidence,
                last-updated: stacks-block-height
            })
        )
        (ok true)
    )
)

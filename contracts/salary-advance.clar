

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-registered (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-advance-limit-reached (err u104))
(define-constant err-repayment-not-due (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-advance-exists (err u107))
(define-constant err-no-advance-to-repay (err u108))
(define-constant err-unauthorized (err u109))
(define-constant err-payday-not-set (err u110))

(define-data-var total-advances uint u0)
(define-data-var total-repayments uint u0)
(define-data-var payday-timestamp uint u0)

(define-constant performance-excellent u80)
(define-constant performance-good u65)
(define-constant performance-average u50)
(define-constant performance-poor u30)

(define-constant err-installment-not-found (err u111))
(define-constant err-installment-already-paid (err u112))
(define-constant err-invalid-installment-count (err u113))
(define-constant err-installment-amount-mismatch (err u114))
(define-constant err-installment-plan-exists (err u115))
(define-constant err-no-installment-plan (err u116))
(define-constant err-installment-overdue (err u117))
(define-constant err-minimum-installment-amount (err u118))

(define-data-var installment-plan-counter uint u0)
(define-data-var total-installment-plans uint u0)
(define-data-var total-installment-payments uint u0)

(define-map installment-plans
  { employee: principal, plan-id: uint }
  {
    total-amount: uint,
    installment-count: uint,
    installment-amount: uint,
    payments-made: uint,
    remaining-balance: uint,
    created-at: uint,
    next-payment-due: uint,
    payment-frequency: uint,
    status: (string-ascii 20),
    late-payment-penalty: uint,
    total-penalty-paid: uint
  }
)

(define-map installment-payments
  { employee: principal, plan-id: uint, payment-id: uint }
  {
    amount: uint,
    payment-date: uint,
    penalty-amount: uint,
    was-late: bool,
    due-date: uint
  }
)

(define-map employee-installment-settings
  { address: principal }
  {
    max-installment-plans: uint,
    preferred-frequency: uint,
    auto-deduct: bool,
    penalty-rate: uint
  }
)

(define-map employees 
  { address: principal }
  {
    salary: uint,
    registered-at: uint,
    total-advances: uint,
    total-repayments: uint,
    active: bool
  }
)

(define-map employee-performance
  { address: principal }
  {
    on-time-repayments: uint,
    late-repayments: uint,
    total-repayments: uint,
    performance-score: uint,
    dynamic-limit: uint
  }
)

(define-map advances
  { employee: principal }
  {
    amount: uint,
    timestamp: uint,
    repaid: bool,
    due-date: uint
  }
)

(define-map employer-funds
  { employer: principal }
  { balance: uint }
)

;; Initialize or update employer funds
(define-public (fund-employer-account (amount uint))
  (let
    (
      (current-balance (default-to u0 (get balance (map-get? employer-funds { employer: tx-sender }))))
      (new-balance (+ current-balance amount))
    )
    (map-set employer-funds { employer: tx-sender } { balance: new-balance })
    (ok new-balance)
  )
)

;; Register an employee with their monthly salary
(define-public (register-employee (employee principal) (salary uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-none (map-get? employees { address: employee })) err-already-registered)
    (asserts! (> salary u0) err-invalid-amount)
    
    (map-set employees 
      { address: employee } 
      {
        salary: salary,
        registered-at: stacks-block-height,
        total-advances: u0,
        total-repayments: u0,
        active: true
      }
    )
    (ok true)
  )
)

;; Update employee salary
(define-public (update-employee-salary (employee principal) (new-salary uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-some (map-get? employees { address: employee })) err-not-registered)
    (asserts! (> new-salary u0) err-invalid-amount)
    
    (map-set employees 
      { address: employee } 
      (merge (unwrap-panic (map-get? employees { address: employee }))
             { salary: new-salary })
    )
    (ok true)
  )
)

;; Set the next payday timestamp
(define-public (set-payday (timestamp uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> timestamp stacks-block-height) err-invalid-amount)
    (var-set payday-timestamp timestamp)
    (ok true)
  )
)

;; Request a salary advance
(define-public (request-advance (amount uint))
  (let
    (
      (employee-data (unwrap! (map-get? employees { address: tx-sender }) err-not-registered))
      (salary (get salary employee-data))
      (max-advance (/ salary u2))
      (payday (var-get payday-timestamp))
    )
    (asserts! (> payday u0) err-payday-not-set)
    (asserts! (is-none (map-get? advances { employee: tx-sender })) err-advance-exists)
    (asserts! (<= amount max-advance) err-advance-limit-reached)
    (asserts! (> amount u0) err-invalid-amount)
    
    (let
      (
        (employer-data (unwrap! (map-get? employer-funds { employer: contract-owner }) err-insufficient-balance))
        (employer-balance (get balance employer-data))
      )
      (asserts! (>= employer-balance amount) err-insufficient-balance)
      
      ;; Update employer balance
      (map-set employer-funds 
        { employer: contract-owner } 
        { balance: (- employer-balance amount) })
      
      ;; Record the advance
      (map-set advances
        { employee: tx-sender }
        {
          amount: amount,
          timestamp: stacks-block-height,
          repaid: false,
          due-date: payday
        }
      )
      
      ;; Update employee stats
      (map-set employees
        { address: tx-sender }
        (merge employee-data 
          { total-advances: (+ (get total-advances employee-data) u1) })
      )
      
      ;; Update global stats
      (var-set total-advances (+ (var-get total-advances) u1))
      
      (ok amount)
    )
  )
)

;; Repay a salary advance
(define-public (repay-advance)
  (let
    (
      (advance-data (unwrap! (map-get? advances { employee: tx-sender }) err-no-advance-to-repay))
      (amount (get amount advance-data))
      (employee-data (unwrap! (map-get? employees { address: tx-sender }) err-not-registered))
    )
    (asserts! (not (get repaid advance-data)) err-no-advance-to-repay)
    
    ;; Update advance status
    (map-set advances
      { employee: tx-sender }
      (merge advance-data { repaid: true })
    )
    
    ;; Update employer funds
    (let
      (
        (employer-data (default-to { balance: u0 } (map-get? employer-funds { employer: contract-owner })))
        (employer-balance (get balance employer-data))
      )
      (map-set employer-funds
        { employer: contract-owner }
        { balance: (+ employer-balance amount) }
      )
    )
    
    ;; Update employee stats
    (map-set employees
      { address: tx-sender }
      (merge employee-data 
        { total-repayments: (+ (get total-repayments employee-data) u1) })
    )
    
    ;; Update global stats
    (var-set total-repayments (+ (var-get total-repayments) u1))
    
    (ok amount)
  )
)

;; Force repayment on payday (called by contract owner)
(define-public (force-repayment (employee principal))
  (let
    (
      (advance-data (unwrap! (map-get? advances { employee: employee }) err-no-advance-to-repay))
      (amount (get amount advance-data))
      (employee-data (unwrap! (map-get? employees { address: employee }) err-not-registered))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (get repaid advance-data)) err-no-advance-to-repay)
    (asserts! (>= stacks-block-height (get due-date advance-data)) err-repayment-not-due)
    
    ;; Update advance status
    (map-set advances
      { employee: employee }
      (merge advance-data { repaid: true })
    )
    
    ;; Update employee stats
    (map-set employees
      { address: employee }
      (merge employee-data 
        { total-repayments: (+ (get total-repayments employee-data) u1) })
    )
    
    ;; Update global stats
    (var-set total-repayments (+ (var-get total-repayments) u1))
    
    (ok amount)
  )
)

;; Get employee data
(define-read-only (get-employee-data (employee principal))
  (map-get? employees { address: employee })
)

;; Get advance data
(define-read-only (get-advance-data (employee principal))
  (map-get? advances { employee: employee })
)

;; Get contract stats
(define-read-only (get-contract-stats)
  {
    total-advances: (var-get total-advances),
    total-repayments: (var-get total-repayments),
    next-payday: (var-get payday-timestamp)
  }
)

;; Check if employee can request an advance
(define-read-only (can-request-advance (employee principal) (amount uint))
  (let
    (
      (employee-data (map-get? employees { address: employee }))
      (advance-data (map-get? advances { employee: employee }))
      (payday (var-get payday-timestamp))
    )
    (if (and 
          (is-some employee-data)
          (is-none advance-data)
          (> payday u0)
          (> amount u0)
          (<= amount (/ (get salary (unwrap-panic employee-data)) u2))
        )
      true
      false
    )
  )
)


(define-map advance-history
  { employee: principal, advance-id: uint }
  {
    amount: uint,
    timestamp: uint,
    repaid: bool,
    due-date: uint,
    repayment-date: uint
  }
)

(define-data-var advance-counter uint u0)


(define-public (record-advance-history (employee principal) (amount uint) (due-date uint))
  (let
    (
      (current-counter (var-get advance-counter))
      (new-counter (+ current-counter u1))
    )
    (map-set advance-history 
      { employee: employee, advance-id: new-counter } 
      {
        amount: amount,
        timestamp: stacks-block-height,
        repaid: false,
        due-date: due-date,
        repayment-date: u0
      }
    )
    (var-set advance-counter new-counter)
    (ok true)
  )
)
(define-public (update-repayment-date (employee principal) (advance-id uint) (repayment-date uint))
  (let
    (
      (advance-data (unwrap! (map-get? advance-history { employee: employee, advance-id: advance-id }) err-no-advance-to-repay))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (get repaid advance-data)) err-no-advance-to-repay)
    
    ;; Update repayment date
    (map-set advance-history
      { employee: employee, advance-id: advance-id }
      (merge advance-data { repayment-date: repayment-date })
    )
    
    (ok true)
  )
)


(define-map employee-limits
  { address: principal }
  { advance-limit-percentage: uint }
)

(define-public (set-employee-limit (employee principal) (limit-percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-some (map-get? employees { address: employee })) err-not-registered)
    (asserts! (<= limit-percentage u100) err-invalid-amount)
    
    (map-set employee-limits
      { address: employee }
      { advance-limit-percentage: limit-percentage }
    )
    (ok true)
  )
)

(define-read-only (get-employee-limit (employee principal))
  (default-to u50 
    (get advance-limit-percentage 
      (map-get? employee-limits { address: employee })))
)



(define-private (calculate-performance-score (on-time uint) (late uint) (total uint))
  (if (is-eq total u0)
    u50
    (let ((on-time-percentage (/ (* on-time u100) total)))
      (if (>= on-time-percentage u90)
        performance-excellent
        (if (>= on-time-percentage u75)
          performance-good
          (if (>= on-time-percentage u60)
            performance-average
            performance-poor))))))

(define-public (initialize-employee-performance (employee principal))
  (begin
    (asserts! (is-some (map-get? employees { address: employee })) err-not-registered)
    (asserts! (is-none (map-get? employee-performance { address: employee })) err-already-registered)
    
    (map-set employee-performance
      { address: employee }
      {
        on-time-repayments: u0,
        late-repayments: u0,
        total-repayments: u0,
        performance-score: u50,
        dynamic-limit: u50
      })
    (ok true)))

(define-public (update-repayment-performance (employee principal) (is-on-time bool))
  (let ((perf-data (default-to 
                     { on-time-repayments: u0, late-repayments: u0, total-repayments: u0, performance-score: u50, dynamic-limit: u50 }
                     (map-get? employee-performance { address: employee }))))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (let ((new-on-time (if is-on-time (+ (get on-time-repayments perf-data) u1) (get on-time-repayments perf-data)))
          (new-late (if is-on-time (get late-repayments perf-data) (+ (get late-repayments perf-data) u1)))
          (new-total (+ (get total-repayments perf-data) u1)))
      
      (let ((new-score (calculate-performance-score new-on-time new-late new-total)))
        (map-set employee-performance
          { address: employee }
          {
            on-time-repayments: new-on-time,
            late-repayments: new-late,
            total-repayments: new-total,
            performance-score: new-score,
            dynamic-limit: new-score
          })
        (ok new-score)))))

(define-read-only (get-employee-performance (employee principal))
  (map-get? employee-performance { address: employee }))

(define-read-only (get-dynamic-advance-limit (employee principal))
  (let ((employee-data (map-get? employees { address: employee }))
        (perf-data (map-get? employee-performance { address: employee })))
    (if (and (is-some employee-data) (is-some perf-data))
      (let ((salary (get salary (unwrap-panic employee-data)))
            (limit-percentage (get dynamic-limit (unwrap-panic perf-data))))
        (/ (* salary limit-percentage) u100))
      u0)))

(define-public (request-performance-based-advance (amount uint))
  (let ((employee-data (unwrap! (map-get? employees { address: tx-sender }) err-not-registered))
        (max-advance (get-dynamic-advance-limit tx-sender))
        (payday (var-get payday-timestamp)))
    
    (asserts! (> payday u0) err-payday-not-set)
    (asserts! (is-none (map-get? advances { employee: tx-sender })) err-advance-exists)
    (asserts! (<= amount max-advance) err-advance-limit-reached)
    (asserts! (> amount u0) err-invalid-amount)
    
    (let ((employer-data (unwrap! (map-get? employer-funds { employer: contract-owner }) err-insufficient-balance))
          (employer-balance (get balance employer-data)))
      
      (asserts! (>= employer-balance amount) err-insufficient-balance)
      
      (map-set employer-funds 
        { employer: contract-owner } 
        { balance: (- employer-balance amount) })
      
      (map-set advances
        { employee: tx-sender }
        {
          amount: amount,
          timestamp: stacks-block-height,
          repaid: false,
          due-date: payday
        })
      
      (map-set employees
        { address: tx-sender }
        (merge employee-data 
          { total-advances: (+ (get total-advances employee-data) u1) }))
      
      (var-set total-advances (+ (var-get total-advances) u1))
      (ok amount))))

(define-read-only (get-employee-advance-capacity (employee principal))
  (let ((employee-data (map-get? employees { address: employee }))
        (advance-data (map-get? advances { employee: employee }))
        (perf-data (map-get? employee-performance { address: employee })))
    
    (if (and (is-some employee-data) (is-none advance-data))
      {
        max-advance: (get-dynamic-advance-limit employee),
        performance-score: (if (is-some perf-data) (get performance-score (unwrap-panic perf-data)) u50),
        limit-percentage: (if (is-some perf-data) (get dynamic-limit (unwrap-panic perf-data)) u50),
        can-request: true
      }
      {
        max-advance: u0,
        performance-score: u0,
        limit-percentage: u0,
        can-request: false
      })))



(define-public (create-installment-plan (total-amount uint) (installment-count uint) (payment-frequency uint))
  (let
    (
      (employee-data (unwrap! (map-get? employees { address: tx-sender }) err-not-registered))
      (salary (get salary employee-data))
      (max-advance (get-dynamic-advance-limit tx-sender))
      (current-counter (var-get installment-plan-counter))
      (new-counter (+ current-counter u1))
      (installment-amount (/ total-amount installment-count))
      (next-payment-due (+ stacks-block-height payment-frequency))
    )
    (asserts! (> total-amount u0) err-invalid-amount)
    (asserts! (and (>= installment-count u2) (<= installment-count u12)) err-invalid-installment-count)
    (asserts! (<= total-amount max-advance) err-advance-limit-reached)
    (asserts! (>= installment-amount u100) err-minimum-installment-amount)
    (asserts! (> payment-frequency u0) err-invalid-amount)
    (asserts! (is-none (map-get? advances { employee: tx-sender })) err-advance-exists)
    
    (let
      (
        (employer-data (unwrap! (map-get? employer-funds { employer: contract-owner }) err-insufficient-balance))
        (employer-balance (get balance employer-data))
      )
      (asserts! (>= employer-balance total-amount) err-insufficient-balance)
      
      (map-set employer-funds 
        { employer: contract-owner } 
        { balance: (- employer-balance total-amount) })
      
      (map-set installment-plans
        { employee: tx-sender, plan-id: new-counter }
        {
          total-amount: total-amount,
          installment-count: installment-count,
          installment-amount: installment-amount,
          payments-made: u0,
          remaining-balance: total-amount,
          created-at: stacks-block-height,
          next-payment-due: next-payment-due,
          payment-frequency: payment-frequency,
          status: "active",
          late-payment-penalty: (/ total-amount u100),
          total-penalty-paid: u0
        }
      )
      
      (map-set advances
        { employee: tx-sender }
        {
          amount: total-amount,
          timestamp: stacks-block-height,
          repaid: false,
          due-date: (+ stacks-block-height (* payment-frequency installment-count))
        }
      )
      
      (map-set employees
        { address: tx-sender }
        (merge employee-data 
          { total-advances: (+ (get total-advances employee-data) u1) })
      )
      
      (var-set installment-plan-counter new-counter)
      (var-set total-installment-plans (+ (var-get total-installment-plans) u1))
      (var-set total-advances (+ (var-get total-advances) u1))
      
      (ok new-counter)
    )
  )
)

(define-public (make-installment-payment (plan-id uint) (payment-amount uint))
  (let
    (
      (plan-data (unwrap! (map-get? installment-plans { employee: tx-sender, plan-id: plan-id }) err-no-installment-plan))
      (expected-amount (get installment-amount plan-data))
      (remaining-balance (get remaining-balance plan-data))
      (payments-made (get payments-made plan-data))
      (installment-count (get installment-count plan-data))
      (next-payment-due (get next-payment-due plan-data))
      (payment-frequency (get payment-frequency plan-data))
      (late-penalty (get late-payment-penalty plan-data))
    )
    (asserts! (is-eq (get status plan-data) "active") err-no-installment-plan)
    (asserts! (> payment-amount u0) err-invalid-amount)
    (asserts! (<= payment-amount remaining-balance) err-invalid-amount)
    
    (let
      (
        (is-late (> stacks-block-height next-payment-due))
        (penalty-amount (if is-late late-penalty u0))
        (total-payment (+ payment-amount penalty-amount))
        (new-payments-made (+ payments-made u1))
        (new-remaining-balance (- remaining-balance payment-amount))
        (new-next-payment-due (+ next-payment-due payment-frequency))
        (is-final-payment (is-eq new-payments-made installment-count))
        (new-status (if is-final-payment "completed" "active"))
      )
      
      (map-set installment-payments
        { employee: tx-sender, plan-id: plan-id, payment-id: new-payments-made }
        {
          amount: payment-amount,
          payment-date: stacks-block-height,
          penalty-amount: penalty-amount,
          was-late: is-late,
          due-date: next-payment-due
        }
      )
      
      (map-set installment-plans
        { employee: tx-sender, plan-id: plan-id }
        (merge plan-data
          {
            payments-made: new-payments-made,
            remaining-balance: new-remaining-balance,
            next-payment-due: (if is-final-payment u0 new-next-payment-due),
            status: new-status,
            total-penalty-paid: (+ (get total-penalty-paid plan-data) penalty-amount)
          }
        )
      )
      
      (let
        (
          (employer-data (default-to { balance: u0 } (map-get? employer-funds { employer: contract-owner })))
          (employer-balance (get balance employer-data))
        )
        (map-set employer-funds
          { employer: contract-owner }
          { balance: (+ employer-balance total-payment) }
        )
      )
      
      (if is-final-payment
        (let
          (
            (advance-data (unwrap! (map-get? advances { employee: tx-sender }) err-no-advance-to-repay))
            (employee-data (unwrap! (map-get? employees { address: tx-sender }) err-not-registered))
          )
          (map-set advances
            { employee: tx-sender }
            (merge advance-data { repaid: true })
          )
          
          (map-set employees
            { address: tx-sender }
            (merge employee-data 
              { total-repayments: (+ (get total-repayments employee-data) u1) })
          )
          
          (var-set total-repayments (+ (var-get total-repayments) u1))
          (ok "plan-completed")
        )
        (begin
          (var-set total-installment-payments (+ (var-get total-installment-payments) u1))
          (ok "payment-processed")
        )
      )
    )
  )
)

(define-public (update-installment-settings (max-plans uint) (preferred-frequency uint) (auto-deduct bool) (penalty-rate uint))
  (begin
    (asserts! (is-some (map-get? employees { address: tx-sender })) err-not-registered)
    (asserts! (and (>= max-plans u1) (<= max-plans u5)) err-invalid-amount)
    (asserts! (> preferred-frequency u0) err-invalid-amount)
    (asserts! (<= penalty-rate u10) err-invalid-amount)
    
    (map-set employee-installment-settings
      { address: tx-sender }
      {
        max-installment-plans: max-plans,
        preferred-frequency: preferred-frequency,
        auto-deduct: auto-deduct,
        penalty-rate: penalty-rate
      }
    )
    (ok true)
  )
)

(define-public (force-installment-payment (employee principal) (plan-id uint) (payment-amount uint))
  (let
    (
      (plan-data (unwrap! (map-get? installment-plans { employee: employee, plan-id: plan-id }) err-no-installment-plan))
      (remaining-balance (get remaining-balance plan-data))
      (payments-made (get payments-made plan-data))
      (installment-count (get installment-count plan-data))
      (next-payment-due (get next-payment-due plan-data))
      (payment-frequency (get payment-frequency plan-data))
      (late-penalty (get late-payment-penalty plan-data))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status plan-data) "active") err-no-installment-plan)
    (asserts! (>= stacks-block-height next-payment-due) err-repayment-not-due)
    (asserts! (<= payment-amount remaining-balance) err-invalid-amount)
    
    (let
      (
        (penalty-amount late-penalty)
        (total-payment (+ payment-amount penalty-amount))
        (new-payments-made (+ payments-made u1))
        (new-remaining-balance (- remaining-balance payment-amount))
        (new-next-payment-due (+ next-payment-due payment-frequency))
        (is-final-payment (is-eq new-payments-made installment-count))
        (new-status (if is-final-payment "completed" "active"))
      )
      
      (map-set installment-payments
        { employee: employee, plan-id: plan-id, payment-id: new-payments-made }
        {
          amount: payment-amount,
          payment-date: stacks-block-height,
          penalty-amount: penalty-amount,
          was-late: true,
          due-date: next-payment-due
        }
      )
      
      (map-set installment-plans
        { employee: employee, plan-id: plan-id }
        (merge plan-data
          {
            payments-made: new-payments-made,
            remaining-balance: new-remaining-balance,
            next-payment-due: (if is-final-payment u0 new-next-payment-due),
            status: new-status,
            total-penalty-paid: (+ (get total-penalty-paid plan-data) penalty-amount)
          }
        )
      )
      
      (let
        (
          (employer-data (default-to { balance: u0 } (map-get? employer-funds { employer: contract-owner })))
          (employer-balance (get balance employer-data))
        )
        (map-set employer-funds
          { employer: contract-owner }
          { balance: (+ employer-balance total-payment) }
        )
      )
      
      (if is-final-payment
        (let
          (
            (advance-data (unwrap! (map-get? advances { employee: employee }) err-no-advance-to-repay))
            (employee-data (unwrap! (map-get? employees { address: employee }) err-not-registered))
          )
          (map-set advances
            { employee: employee }
            (merge advance-data { repaid: true })
          )
          
          (map-set employees
            { address: employee }
            (merge employee-data 
              { total-repayments: (+ (get total-repayments employee-data) u1) })
          )
          
          (var-set total-repayments (+ (var-get total-repayments) u1))
          (ok "plan-completed")
        )
        (begin
          (var-set total-installment-payments (+ (var-get total-installment-payments) u1))
          (ok "payment-processed")
        )
      )
    )
  )
)

(define-read-only (get-installment-plan (employee principal) (plan-id uint))
  (map-get? installment-plans { employee: employee, plan-id: plan-id })
)

(define-read-only (get-installment-payment (employee principal) (plan-id uint) (payment-id uint))
  (map-get? installment-payments { employee: employee, plan-id: plan-id, payment-id: payment-id })
)

(define-read-only (get-employee-installment-settings (employee principal))
  (default-to 
    { max-installment-plans: u3, preferred-frequency: u100, auto-deduct: false, penalty-rate: u1 }
    (map-get? employee-installment-settings { address: employee })
  )
)

(define-read-only (get-installment-stats)
  {
    total-installment-plans: (var-get total-installment-plans),
    total-installment-payments: (var-get total-installment-payments),
    current-plan-counter: (var-get installment-plan-counter)
  }
)

(define-read-only (calculate-installment-schedule (total-amount uint) (installment-count uint) (payment-frequency uint))
  (let
    (
      (installment-amount (/ total-amount installment-count))
      (total-duration (* installment-count payment-frequency))
      (final-payment-adjustment (- total-amount (* installment-amount installment-count)))
    )
    {
      installment-amount: installment-amount,
      total-duration: total-duration,
      final-payment-amount: (+ installment-amount final-payment-adjustment),
      estimated-completion: (+ stacks-block-height total-duration)
    }
  )
)

(define-read-only (get-overdue-installments (employee principal) (plan-id uint))
  (let
    (
      (plan-data (map-get? installment-plans { employee: employee, plan-id: plan-id }))
    )
    (if (is-some plan-data)
      (let
        (
          (plan (unwrap-panic plan-data))
          (next-due (get next-payment-due plan))
          (is-overdue (and (> stacks-block-height next-due) (is-eq (get status plan) "active")))
        )
        {
          is-overdue: is-overdue,
          days-overdue: (if is-overdue (- stacks-block-height next-due) u0),
          penalty-amount: (if is-overdue (get late-payment-penalty plan) u0),
          remaining-balance: (get remaining-balance plan)
        }
      )
      {
        is-overdue: false,
        days-overdue: u0,
        penalty-amount: u0,
        remaining-balance: u0
      }
    )
  )
)

;; Credit Score System Constants
(define-constant err-credit-score-not-found (err u119))
(define-constant err-invalid-credit-score (err u120))
(define-constant err-credit-assessment-failed (err u121))
(define-constant err-insufficient-credit-history (err u122))

;; Credit score ranges and thresholds
(define-constant credit-score-excellent u850)
(define-constant credit-score-very-good u740)
(define-constant credit-score-good u670)
(define-constant credit-score-fair u580)
(define-constant credit-score-poor u300)

(define-data-var credit-assessment-counter uint u0)

;; Employee credit profiles with comprehensive scoring
(define-map employee-credit-profiles
  { address: principal }
  {
    current-score: uint,
    score-history-count: uint,
    salary-to-advance-ratio: uint,
    payment-consistency-score: uint,
    advance-frequency-score: uint,
    tenure-bonus: uint,
    risk-factor: uint,
    last-assessment: uint,
    credit-limit-multiplier: uint,
    interest-rate-modifier: uint
  }
)

;; Historical credit score tracking
(define-map credit-score-history
  { employee: principal, assessment-id: uint }
  {
    score: uint,
    assessment-date: uint,
    factors: {
      salary-stability: uint,
      payment-history: uint,
      advance-pattern: uint,
      tenure: uint,
      performance: uint
    },
    recommendations: (string-ascii 200),
    trend: (string-ascii 20)
  }
)

;; Credit score factors and weights
(define-map credit-scoring-factors
  { factor-type: (string-ascii 30) }
  {
    weight: uint,
    max-score: uint,
    min-score: uint,
    description: (string-ascii 100)
  }
)

;; Initialize credit scoring system with default factors
(define-public (initialize-credit-system)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Set up scoring factors with weights
    (map-set credit-scoring-factors 
      { factor-type: "salary-stability" }
      { weight: u30, max-score: u300, min-score: u0, description: "Consistent salary payments and job tenure" })
    
    (map-set credit-scoring-factors 
      { factor-type: "payment-history" }
      { weight: u35, max-score: u350, min-score: u0, description: "On-time advance repayments and consistency" })
    
    (map-set credit-scoring-factors 
      { factor-type: "advance-pattern" }
      { weight: u20, max-score: u200, min-score: u0, description: "Responsible advance usage patterns" })
    
    (map-set credit-scoring-factors 
      { factor-type: "tenure" }
      { weight: u10, max-score: u100, min-score: u0, description: "Length of employment relationship" })
    
    (map-set credit-scoring-factors 
      { factor-type: "performance" }
      { weight: u5, max-score: u50, min-score: u0, description: "Employee performance metrics" })
    
    (ok true)
  )
)

;; Calculate comprehensive credit score
(define-public (calculate-credit-score (employee principal))
  (let
    (
      (employee-data (unwrap! (map-get? employees { address: employee }) err-not-registered))
      (perf-data (map-get? employee-performance { address: employee }))
      (current-counter (var-get credit-assessment-counter))
      (new-counter (+ current-counter u1))
    )
    
    ;; Calculate individual scoring factors
    (let
      (
        (salary-score (calculate-salary-stability-score employee employee-data))
        (payment-score (calculate-payment-history-score employee perf-data))
        (advance-score (calculate-advance-pattern-score employee employee-data))
        (tenure-score (calculate-tenure-score employee-data))
        (performance-score (calculate-performance-factor-score perf-data))
        (total-score (+ salary-score (+ payment-score (+ advance-score (+ tenure-score performance-score)))))
      )
      
      ;; Calculate additional metrics
      (let
        (
          (salary-ratio (calculate-salary-to-advance-ratio employee-data))
          (consistency-score (if (is-some perf-data) 
                              (get performance-score (unwrap-panic perf-data)) u50))
          (frequency-score (calculate-advance-frequency-score employee-data))
          (tenure-bonus (/ tenure-score u10))
          (risk-factor (calculate-risk-factor total-score))
          (credit-multiplier (calculate-credit-limit-multiplier total-score))
          (interest-modifier (calculate-interest-rate-modifier total-score))
        )
        
        ;; Update employee credit profile
        (map-set employee-credit-profiles
          { address: employee }
          {
            current-score: total-score,
            score-history-count: (+ (default-to u0 
              (get score-history-count (map-get? employee-credit-profiles { address: employee }))) u1),
            salary-to-advance-ratio: salary-ratio,
            payment-consistency-score: consistency-score,
            advance-frequency-score: frequency-score,
            tenure-bonus: tenure-bonus,
            risk-factor: risk-factor,
            last-assessment: stacks-block-height,
            credit-limit-multiplier: credit-multiplier,
            interest-rate-modifier: interest-modifier
          }
        )
        
        ;; Record credit score history
        (map-set credit-score-history
          { employee: employee, assessment-id: new-counter }
          {
            score: total-score,
            assessment-date: stacks-block-height,
            factors: {
              salary-stability: salary-score,
              payment-history: payment-score,
              advance-pattern: advance-score,
              tenure: tenure-score,
              performance: performance-score
            },
            recommendations: (generate-credit-recommendations total-score),
            trend: (determine-credit-trend employee total-score)
          }
        )
        
        (var-set credit-assessment-counter new-counter)
        (ok total-score)
      )
    )
  )
)

;; Calculate salary stability score based on tenure and salary consistency
(define-private (calculate-salary-stability-score (employee principal) (employee-data { salary: uint, registered-at: uint, total-advances: uint, total-repayments: uint, active: bool }))
  (let
    (
      (tenure-blocks (- stacks-block-height (get registered-at employee-data)))
      (salary (get salary employee-data))
      (base-score u150)
    )
    ;; Higher salary and longer tenure increase stability score
    (+ base-score 
       (/ tenure-blocks u1000) 
       (/ salary u10000))
  )
)

;; Calculate payment history score based on repayment behavior
(define-private (calculate-payment-history-score (employee principal) (perf-data (optional { on-time-repayments: uint, late-repayments: uint, total-repayments: uint, performance-score: uint, dynamic-limit: uint })))
  (if (is-some perf-data)
    (let
      (
        (perf (unwrap-panic perf-data))
        (on-time (get on-time-repayments perf))
        (total (get total-repayments perf))
        (base-score u175)
      )
      (if (> total u0)
        (+ base-score (/ (* on-time u150) total))
        base-score
      )
    )
    u175 ;; Default score for new employees
  )
)

;; Calculate advance pattern score based on responsible usage
(define-private (calculate-advance-pattern-score (employee principal) (employee-data { salary: uint, registered-at: uint, total-advances: uint, total-repayments: uint, active: bool }))
  (let
    (
      (employee-advances (get total-advances employee-data))
      (salary (get salary employee-data))
      (base-score u100)
    )
    ;; Reward moderate usage relative to salary
    (if (> employee-advances u0)
      (let ((usage-ratio (/ (* employee-advances u100) salary)))
        (if (<= usage-ratio u10)
          (+ base-score u90) ;; Excellent usage
          (if (<= usage-ratio u25)
            (+ base-score u60) ;; Good usage
            (+ base-score u30)))) ;; Fair usage
      (+ base-score u50) ;; No usage yet
    )
  )
)

;; Calculate tenure score based on employment length
(define-private (calculate-tenure-score (employee-data { salary: uint, registered-at: uint, total-advances: uint, total-repayments: uint, active: bool }))
  (let
    (
      (tenure-blocks (- stacks-block-height (get registered-at employee-data)))
      (base-score u25)
    )
    ;; Reward longer tenure
    (+ base-score (/ tenure-blocks u2000))
  )
)

;; Calculate performance factor score
(define-private (calculate-performance-factor-score (perf-data (optional { on-time-repayments: uint, late-repayments: uint, total-repayments: uint, performance-score: uint, dynamic-limit: uint })))
  (if (is-some perf-data)
    (/ (get performance-score (unwrap-panic perf-data)) u2)
    u25
  )
)

;; Calculate salary to advance ratio
(define-private (calculate-salary-to-advance-ratio (employee-data { salary: uint, registered-at: uint, total-advances: uint, total-repayments: uint, active: bool }))
  (let
    (
      (salary (get salary employee-data))
      (total-advances-count (get total-advances employee-data))
    )
    (if (> total-advances-count u0)
      (/ salary total-advances-count)
      salary
    )
  )
)

;; Calculate advance frequency score
(define-private (calculate-advance-frequency-score (employee-data { salary: uint, registered-at: uint, total-advances: uint, total-repayments: uint, active: bool }))
  (let
    (
      (tenure-blocks (- stacks-block-height (get registered-at employee-data)))
      (advance-count (get total-advances employee-data))
    )
    (if (> tenure-blocks u0)
      (let ((frequency (/ advance-count tenure-blocks)))
        (if (<= frequency u1) u90 ;; Excellent frequency
          (if (<= frequency u3) u70 ;; Good frequency
            u40))) ;; High frequency
      u50
    )
  )
)

;; Calculate risk factor based on total score
(define-private (calculate-risk-factor (total-score uint))
  (if (>= total-score credit-score-very-good) u10 ;; Low risk
    (if (>= total-score credit-score-good) u25 ;; Medium-low risk
      (if (>= total-score credit-score-fair) u50 ;; Medium risk
        u80))) ;; High risk
)

;; Calculate credit limit multiplier based on score
(define-private (calculate-credit-limit-multiplier (total-score uint))
  (if (>= total-score credit-score-excellent) u200 ;; 2x multiplier
    (if (>= total-score credit-score-very-good) u150 ;; 1.5x multiplier
      (if (>= total-score credit-score-good) u120 ;; 1.2x multiplier
        (if (>= total-score credit-score-fair) u100 ;; 1x multiplier
          u80)))) ;; 0.8x multiplier
)

;; Calculate interest rate modifier based on score
(define-private (calculate-interest-rate-modifier (total-score uint))
  (if (>= total-score credit-score-excellent) u50 ;; 50% of base rate
    (if (>= total-score credit-score-very-good) u75 ;; 75% of base rate
      (if (>= total-score credit-score-good) u100 ;; 100% of base rate
        (if (>= total-score credit-score-fair) u125 ;; 125% of base rate
          u150)))) ;; 150% of base rate
)

;; Generate credit improvement recommendations
(define-private (generate-credit-recommendations (score uint))
  (if (>= score credit-score-very-good)
    "Excellent credit! Continue consistent repayment patterns"
    (if (>= score credit-score-good)
      "Good credit. Focus on reducing advance frequency for improvement"
      (if (>= score credit-score-fair)
        "Fair credit. Improve on-time payments and reduce advance amounts"
        "Poor credit. Focus on timely repayments and stable employment")))
)

;; Determine credit trend based on historical data
(define-private (determine-credit-trend (employee principal) (current-score uint))
  (let
    (
      (profile (map-get? employee-credit-profiles { address: employee }))
    )
    (if (is-some profile)
      (let
        (
          (history-count (get score-history-count (unwrap-panic profile)))
        )
        (if (> history-count u1)
          "stable" ;; Could implement more sophisticated trending
          "new"))
      "new")
  )
)

;; Get credit-based advance limit for employee
(define-public (get-credit-based-advance-limit (employee principal))
  (let
    (
      (employee-data (map-get? employees { address: employee }))
      (credit-profile (map-get? employee-credit-profiles { address: employee }))
    )
    (if (and (is-some employee-data) (is-some credit-profile))
      (let
        (
          (salary (get salary (unwrap-panic employee-data)))
          (multiplier (get credit-limit-multiplier (unwrap-panic credit-profile)))
          (base-limit (/ salary u2))
        )
        (ok (/ (* base-limit multiplier) u100))
      )
      err-credit-score-not-found
    )
  )
)

;; Read-only functions for credit data access
(define-read-only (get-employee-credit-profile (employee principal))
  (map-get? employee-credit-profiles { address: employee })
)

(define-read-only (get-credit-score-history (employee principal) (assessment-id uint))
  (map-get? credit-score-history { employee: employee, assessment-id: assessment-id })
)

(define-read-only (get-credit-scoring-factor (factor-type (string-ascii 30)))
  (map-get? credit-scoring-factors { factor-type: factor-type })
)

(define-read-only (get-credit-assessment-stats)
  {
    total-assessments: (var-get credit-assessment-counter),
    system-initialized: (is-some (map-get? credit-scoring-factors { factor-type: "salary-stability" }))
  }
)


  
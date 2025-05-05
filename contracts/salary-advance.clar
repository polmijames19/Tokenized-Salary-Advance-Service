

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
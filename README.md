# Tokenized Salary Advance Service

A Clarity smart contract that enables employees to access a portion of their earnings before the regular payday, with built-in repayment logic.

## Overview

This contract allows:
- Employers to register employees and their salaries
- Employees to request advances up to 50% of their monthly salary
- Automatic tracking of advances and repayments
- Employers to enforce repayments on payday

## Contract Functions

### For Employers

- `fund-employer-account(amount)`: Add funds to the employer's account
- `register-employee(employee, salary)`: Register a new employee with their salary
- `update-employee-salary(employee, new-salary)`: Update an employee's salary
- `set-payday(timestamp)`: Set the next payday timestamp
- `force-repayment(employee)`: Force repayment of an advance on payday

### For Employees

- `request-advance(amount)`: Request a salary advance (up to 50% of monthly salary)
- `repay-advance()`: Repay an outstanding salary advance

### Read-Only Functions

- `get-employee-data(employee)`: Get employee registration data
- `get-advance-data(employee)`: Get details about an employee's current advance
- `get-contract-stats()`: Get overall contract statistics
- `can-request-advance(employee, amount)`: Check if an employee can request an advance

## Usage Example

```clarity
;; As the employer
(contract-call? .salary-advance fund-employer-account u10000000)
(contract-call? .salary-advance register-employee 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM u10000)
(contract-call? .salary-advance set-payday u100000)

;; As an employee
(contract-call? .salary-advance request-advance u5000)
(contract-call? .salary-advance repay-advance)

;; Check status
(contract-call? .salary-advance get-employee-data 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

## Error Codes

- `u100`: Not contract owner
- `u101`: Employee not registered
- `u102`: Employee already registered
- `u103`: Insufficient balance
- `u104`: Advance limit reached
- `u105`: Repayment not due yet
- `u106`: Invalid amount
- `u107`: Advance already exists
- `u108`: No advance to repay
- `u109`: Unauthorized operation
- `u110`: Payday not set
```

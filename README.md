# Ad Server Submission RObot HaCK
Or ASSROCK :=)

## Setup:

Prerequisite:
You need a PostgreSQL database

1. Install [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
2. Build: `stack build`
3. Run tests: `stack test`
4. Run app: `db="host=localhost dbname=test" port=3000 stack exec submission-robot-hack-exe`

Run the test first, because it automatically creates the necessary SQL tables (`msisdn_submissions` and `pin_submissions`)

## Usage:

```bash
curl "http://localhost:3001/submit_msisdn/m.mobiworld.biz/gr/mobilearts/841/?msisdn=306972865041"

curl "http://localhost:3001/submit_pin/?sid=kw==&pin=5192"
```

## How It works

```
MSISDN Submission:
http://m.mobiworld.biz/gr/mobilearts?country=gr&handle=mobilearts&device=smart&msisdnSubmitted=Y&incentivizedCheckbox=Y&legalCheckbox=N&legalCheckbox=Y&op_confirmCheckbox=N&offer=1&msisdn%5B0%5D=6972865041

Response is invalid if it contains "numeric-field msisdn msisdn-input msisdn-input-0"

----

PIN Submission:
http://m.mobiworld.biz/gr/mobilearts?country=gr&handle=mobilearts&offer=1&device=smart&gaclientid=&msisdnSubmitted=Y&msisdn%5B0%5D=6972865367&incentivizedCheckbox=Y&legalCheckbox=Y&op_confirmCheckbox=N&identified=1&operator=GR_VODAFONE&rid=243536df8b33478dab05d132c8f05766&pinSubmitted=Y&pin=7826

Response is invalid if it contains "numeric-field pin pin-input"
```

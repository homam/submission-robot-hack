# API Usage - Mobile Arts

## Submit a MSISDN to Send a PIN
```bash
$ msisdn="6972865474" &&
  domainHost="m.mobiworld.biz" &&
  country="gr" &&
  handle="mobilearts" &&
  curl "http://lp-api.sam-media.com/v1/submit_msisdn/$domainHost/$country/$handle/841/?msisdn=$msisdn"
```

The API returns a JSON object with `submissionId`, `isValid` and `errorText` fields.

Invalid MSISDN (error):
```json
{"submissionId":"lcU=","errorText":null,"isValid":true} 
```

MSISDN was successfully submitted and PIN was sent to the user:
```json
{"submissionId":"lcU=","errorText":null,"isValid":true}
```

## Validate PIN and Finalize Subscription

```bash
$ sid="lcw=" && 
  pin="8575" && 
  curl "http://lp-api.sam-media.com/v1/submit_pin/?sid=$sid&pin=$pin"
```

When the PIN submission is invalid, the API returns 

```json
{"erroText":"Validation Failed","isValid":false, "submissionId": "k8s="}
```

And if submission was valid and sale is completed the API returns:

```json
{"finalUrl":"http://gr.mobiworldbiz.com/?uid=fdf098fcc6&uip=2.84.0.0","submissionId":"k8s=","errorText":null,"isValid":true}
```

`finalUrl` is the access URL for the portal.

## Check existance of a MSISDN

```bash
$ country="gr" &&
  msisdn="306972865474" && 
  curl "http://lp-api.sam-media.com/v1/check_msisdn_active_subscription/$country/?msisdn=$msisdn"
```

Returns either `{"isActive":false}` or `{"isActive":true}`.
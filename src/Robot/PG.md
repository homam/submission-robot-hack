curl 'http://wap.trend-tech.net/landings/subscribe' -H 'Connection: keep-alive' -H 'Cache-Control: max-age=0' -H 'Origin: http://wap.trend-tech.net' -H 'Upgrade-Insecure-Requests: 1' -H 'Content-Type: application/x-www-form-urlencoded' -H 'User-Agent: Mozilla/5.0 (Linux; U; Android 4.0; en-us; GT-I9300 Build/IMM76D) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' -H 'Referer: http://wap.trend-tech.net/landings/mobilicerik/love-test-exclusive?mcb_sid=516773a7cb5544a3bec0bb05b8f466ed&scenario_id=674&affid=PTL&type=MCB_NM&rockman_id=e831bf006b674f748bb60902c78f5b80' -H 'Accept-Encoding: gzip, deflate' -H 'Accept-Language: en-US,en;q=0.9' -H 'Cookie: _ga=GA1.1.1120653505.1485874178; _ga=GA1.2.1120653505.1485874178; PHPSESSID=r070ksiidrev4ra9915fk01cu5; _gid=GA1.2.1351631409.1529396744' --data 'msisdn=5372617227&servicekey=6f518c84a2f639457d5c3589bb4f90a6&ccount=1&ckey=&skey=0jo1SU2fk3PX4z45MS6v27Y38xz9XZaqvBSU' --compressed



curl 'http://wap.trend-tech.net/landings/confirm-otp' -H 'Cookie: PHPSESSID=9bjeht5d4pu8lnefric5e67d94; _ga=GA1.2.1498569402.1529415000; _gid=GA1.2.1024581695.1529415000' -H 'Origin: http://wap.trend-tech.net' -H 'Accept-Encoding: gzip, deflate' -H 'Accept-Language: en-US,en;q=0.9' -H 'User-Agent: Mozilla/5.0 (Linux; U; Android 4.0; en-us; GT-I9300 Build/IMM76D) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30' -H 'Content-Type: application/x-www-form-urlencoded; charset=UTF-8' -H 'Accept: */*' -H 'Referer: http://wap.trend-tech.net/landings/confirm-otp' -H 'X-Requested-With: XMLHttpRequest' -H 'Connection: keep-alive' --data 'otp=2248' --compressed


curl 'http://nghttp2.org/httpbin/post' -H 'Connection: keep-alive' -H 'Cache-Control: max-age=0' -H 'Origin: http://wap.trend-tech.net' -H 'Upgrade-Insecure-Requests: 1' -H 'Content-Type: application/x-www-form-urlencoded' -H 'User-Agent: Mozilla/5.0 (Linux; U; Android 4.0; en-us; GT-I9300 Build/IMM76D) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' -H 'Referer: http://wap.trend-tech.net/landings/mobilicerik/love-test-exclusive?mcb_sid=516773a7cb5544a3bec0bb05b8f466ed&scenario_id=674&affid=PTL&type=MCB_NM&rockman_id=e831bf006b674f748bb60902c78f5b80' -H 'Accept-Encoding: gzip, deflate' -H 'Accept-Language: en-US,en;q=0.9' -H 'Cookie: _ga=GA1.1.1120653505.1485874178; _ga=GA1.2.1120653505.1485874178; PHPSESSID=r070ksiidrev4ra9915fk01cu5; _gid=GA1.2.1351631409.1529396744' --data 'msisdn=5372617227&servicekey=6f518c84a2f639457d5c3589bb4f90a6&ccount=1&ckey=&skey=0jo1SU2fk3PX4z45MS6v27Y38xz9XZaqvBSU' --compressed




```
        current  = [
          '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
          'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'R', 'S', 'Q', 'W', 'X', 'T', 'U', 'V', 'Y', 'Z',
          '$', '#', '+', '-', '?', ':', '.', ';', '_', '~'];
          
        var skeyElement = document['getElementById']('skey');
        document['getElementById']('ccount')['value'] = 1;
        var servicekey = document['getElementById']('servicekey')['value'];


        height = 1;
        str = servicekey['split']('')['reverse']()['join']('');

        var ret = '';
        var secs = '';

        var Y = 0;
        for (; Y < str.length; Y++) {
          var topIn = str['charCodeAt'](Y);
          secs = current[Y] + current[(Y + topIn + height * 1) % 36] + current[(Y + 5 + topIn + height * 1) % 36];
          if (Y % 2 == 0) {
            secs = secs['toLowerCase']();
          }
          ret = ret + (secs + '');
        }

        skeyElement['value'] = ret['substring'](0, 36);
```
Push Notification Service (pnsvc)
=====

## Overview

TBD

## Usage

```erlang
1> pnsvc:send(Request, GCPProjectId).

{ok, 200, ...}
```

### About Request

[FCM App Server Protocols HTTP v1 API.](https://firebase.google.com/docs/reference/fcm/rest/v1/projects.messages?hl=ja)

- for example(android)

```erlang
#{
    message => #{
        notification => #{
            title => <<"notification title"/utf8>>,
            body => <<"notification body"/utf8>>
        },
        token => <<"fcm registration token">>,
        android => #{
            ttl => "3600s"
        }
    }
}.
```

- for example(ios)

```erlang
TBD
```

- for example(webpush)

```erlang
TBD
```

### Push Notification test

- curl
```
$ curl -XPOST -H 'Content-Type: application/json' \
    -d '{"message": 
            {
                "token": "dR1Xd5PcL...xuyntS",
                "data": {
                    "title": "notification title",
                    "body": "notification body",
                    "icon": "icon image",
                    "badge": "icon image url displayed at the device status bar"
                }
            }
        }' http://localhost:5000/v1/projects/:project_id/send | jq .
```

`:project_id` is your firebase project id.

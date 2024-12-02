# EMQX Enterprise Plugin for Retained Messages Lazy Push

This is a plugin for EMQX Enterprise Edition which provides a lazy push mechanism for retained messages.

## MQTT How Bridge Works

When we need to bridge two brokers (or clusters), there has to be at least one MQTT client connected to both sides of the bridge.
Depending on the desired message forwarding direction, the client may subscribe to one side and forwards messages to the other side,
or it may subscribe to both sides and forwards messages in both directions.

To avoid message loss, the bridge client may choose to use persistent sessions (session expiry > 0),
so the broker can buffer the messages while the connection is down and resend them when the connection is up.

A lot of the brokers (including EMQX) support the MQTT bridge feature, which embeds the bridge client in the broker itself,
but they are essentially the same as the client-based bridge in terms of message forwarding.

The regular MQTT bridge in EMQX is a bit, although it does not create a local subscripiton, it still keeps the messages
to be sent in a buffer, which essentially is the same as the session state.

## Challenge of Retained Messages

Session state or send buffer is helpful in most cases for regular MQTT messages, but it is not efficient for retained messages.
The idea of retained messages is to keep the last message for a topic, so when a new client subscribes to the topic, it can get the last message immediately.
If the retained message is buffered in the session state, the old retained message will still be sent to the client after the connection is up, instead of the latest one.

## How Retained Messages Lazy Push Works

This plugin provides a lazy push mechanism for retained messages, which means the broker client will try to buffer regular messages in the send buffer,
but it will not buffer retained messages when connection is down. Instead, it will push the retained messages to the client immediately after the connection is up.

## Release

An EMQX plugin release is a tar file including including a subdirectory of this plugin's name and it's version, that contains:

1. A JSON format metadata file describing the plugin
2. Versioned directories for all applications needed for this plugin (source and binaries).

In a shell from this plugin's working directory execute `make rel` to have the package created like:

```
_build/default/emqx_rpush/emqx_rpush-<vsn>.tar.gz
```

See [EMQX documentation](https://docs.emqx.com/en/enterprise/v5.0/extensions/plugins.html) for details on how to deploy custom plugins.

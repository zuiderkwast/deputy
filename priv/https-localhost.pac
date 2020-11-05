/*
 * Uses localhost as HTTPS proxy for all hosts except for itself.
 *
 * In your browser, put the URI (file:///...) to this file under
 * the network setting "Automatic proxy configuration URL".
 */
function FindProxyForURL(url, host) {
    if (host == "localhost") {
        return null;
    } else {
        return "HTTPS localhost:8443";
    }
}
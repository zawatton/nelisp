// DTW Android shell (Doc 165 Track 1 W1).
//
// Hosts the unmodified site/dtw wasm bundle in a WebView.  The bundle is
// served through WebViewAssetLoader's virtual https origin instead of
// file:///android_asset because file:// gives a null origin and no
// application/wasm MIME, which breaks fetch()-based wasm loading.
package dev.nelisp.dtw

import android.annotation.SuppressLint
import android.app.Activity
import android.os.Bundle
import android.webkit.WebResourceRequest
import android.webkit.WebResourceResponse
import android.webkit.WebView
import android.webkit.WebViewClient
import androidx.webkit.WebViewAssetLoader

class MainActivity : Activity() {
    @SuppressLint("SetJavaScriptEnabled")
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val assetLoader = WebViewAssetLoader.Builder()
            .addPathHandler("/assets/", WebViewAssetLoader.AssetsPathHandler(this))
            .build()
        val webView = WebView(this)
        webView.settings.javaScriptEnabled = true
        // The asset loader replaces raw file:// access entirely.
        webView.settings.allowFileAccess = false
        webView.settings.allowContentAccess = false
        webView.webViewClient = object : WebViewClient() {
            override fun shouldInterceptRequest(
                view: WebView,
                request: WebResourceRequest,
            ): WebResourceResponse? = assetLoader.shouldInterceptRequest(request.url)
        }
        setContentView(webView)
        webView.loadUrl("https://appassets.androidplatform.net/assets/dtw/index.html")
    }
}

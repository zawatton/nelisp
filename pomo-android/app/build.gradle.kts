plugins {
    id("com.android.application")
    id("org.jetbrains.kotlin.android")
}

android {
    namespace = "dev.nelisp.pomo"
    compileSdk = 35

    defaultConfig {
        applicationId = "dev.nelisp.pomo"
        // Doc 165 W1: minSdk 24 (Android 7.0) -- the real gate is the
        // Play-updatable System WebView (Chromium), not the OS API level.
        minSdk = 24
        targetSdk = 35
        versionCode = 1
        versionName = "0.1.0"
    }

    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }
    kotlinOptions {
        jvmTarget = "17"
    }
}

dependencies {
    // WebViewAssetLoader: serves the bundled site over a secure virtual
    // origin with correct MIME types (Doc 165 2.2).
    implementation("androidx.webkit:webkit:1.12.1")
}

utils = load 'ci/utils.groovy'
cmn = load 'ci/common.groovy'

packageFolder = './StatusImPackage'

def cleanupAndDeps() {
  sh 'make clean'
  sh 'cp .env.jenkins .env'
  utils.nix_sh 'lein deps'
  utils.installJSDeps('desktop')
}

def buildClojureScript() {
  utils.nix_sh '''
    make prod-build-desktop && \
    ./scripts/build-desktop.sh buildClojureScript
  '''
}

def uploadArtifact(filename) {
  def domain = 'ams3.digitaloceanspaces.com'
  def bucket = 'status-im-desktop'
  withCredentials([usernamePassword(
    credentialsId: 'digital-ocean-access-keys',
    usernameVariable: 'DO_ACCESS_KEY',
    passwordVariable: 'DO_SECRET_KEY'
  )]) {
    sh """
      s3cmd \\
        --acl-public \\
        --host='${domain}' \\
        --host-bucket='%(bucket)s.${domain}' \\
        --access_key=${DO_ACCESS_KEY} \\
        --secret_key=${DO_SECRET_KEY} \\
        put ${filename} s3://${bucket}/
    """

  }
  def url = "https://${bucket}.${domain}/${filename}"
  return url
}

/* MAIN --------------------------------------------------*/

def compile() {
  /* disable logs for desktop builds when releasing */
  if (params.BUILD_TYPE == 'release') {
    env.STATUS_NO_LOGGING = 1
  }
  /* since QT is in a custom path we need to add it to PATH */
  if (env.QT_PATH) {
    env.PATH = "${env.QT_PATH}:${env.PATH}"
  }
  utils.nix_sh './scripts/build-desktop.sh compile'
}

def bundleWindows(type = 'nightly') {
  def pkg

  utils.nix_sh './scripts/build-desktop.sh bundle'
  dir(packageFolder) {
    pkg = utils.pkgFilename(type, 'exe')
    sh "mv ../Status-x86_64-setup.exe ${pkg}"
  }
  return "${packageFolder}/${pkg}".drop(2)
}

def bundleLinux(type = 'nightly') {
  def pkg
  utils.nix_sh './scripts/build-desktop.sh bundle'
  dir(packageFolder) {
    pkg = utils.pkgFilename(type, 'AppImage')
    sh "mv ../Status-x86_64.AppImage ${pkg}"
  }
  return "${packageFolder}/${pkg}".drop(2)
}

def bundleMacOS(type = 'nightly') {
  def pkg = utils.pkgFilename(type, 'dmg')
  utils.nix_sh './scripts/build-desktop.sh bundle'
  dir(packageFolder) {
    withCredentials([
      string(credentialsId: 'desktop-gpg-outer-pass', variable: 'GPG_PASS_OUTER'),
      string(credentialsId: 'desktop-gpg-inner-pass', variable: 'GPG_PASS_INNER'),
      string(credentialsId: 'desktop-keychain-pass', variable: 'KEYCHAIN_PASS')
    ]) {
      utils.nix_sh """
        ../scripts/sign-macos-pkg.sh Status.app ../deployment/macos/macos-developer-id.keychain-db.gpg && \
        ../node_modules/appdmg/bin/appdmg.js ../deployment/macos/status-dmg.json ${pkg} && \
        ../scripts/sign-macos-pkg.sh ${pkg} ../deployment/macos/macos-developer-id.keychain-db.gpg
      """
    }
  }
  return "${packageFolder}/${pkg}".drop(2)
}

return this

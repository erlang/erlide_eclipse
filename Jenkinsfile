#!groovy

pipeline {
	agent any
	options {
		disableConcurrentBuilds()
		timestamps()
		skipDefaultCheckout()
		buildDiscarder(logRotator(numToKeepStr: '10'))
	}
	stages {
		stage('Checkout') {
			steps{
				retry(3) {
					timeout(time: 30, unit: 'SECONDS') {
						script {
							checkout()
						}
					}
				}
			}
		}

		stage('Compile') {
			steps{
				script {
					compile()
					analyze()
				}
			}
		}

		stage('Archive') {
			steps{
				script {
					archive = archive()
				}
			}
		}

		stage('Publish') {
			steps{
				script {
					publish(archive)
					publishRelease(archive)
				}
			}
		}
	}
	post {
		always {
			deleteDir()
		}
	}

}

///////////////////////////////////

def checkout() {
	deleteDir()
	checkout([
			$class: 'GitSCM',
			branches: scm.branches,
			extensions: scm.extensions + [[$class: 'CleanCheckout'], [$class: 'CloneOption', depth: 100, noTags: false, reference: '', shallow: true]],
			userRemoteConfigs: scm.userRemoteConfigs
			])

    sh('git rev-parse HEAD > GIT_COMMIT')
    git_commit=readFile('GIT_COMMIT')
    def short_commit=git_commit.take(6)

	//currentBuild.setName("${short_commit}__${env.BUILD_NUMBER}")
	currentBuild.setDescription("${env.BRANCH_NAME} - ${short_commit}")
}

def compile() {
	sh "chmod u+x mvnw"
	def product
	if(env.BRANCH_NAME=="master")
		product=",build-product"
	else
		product=""
	profiles="help${product}"
	wrap([$class: 'Xvfb', displayNameOffset: 100, installationName: 'xvfb', screen: '1024x768x24']) {
		sh "PATH=$PATH:~jenkins/erlide_tools && ./mvnw -B -U clean verify -P ${profiles} -Dmaven.test.failure.ignore=true"
	}
	if(env.BRANCH_NAME=="master") {
		// TODO rename product artifacts
	}
}

def analyze() {
	step([$class: 'WarningsPublisher', canComputeNew: false, canResolveRelativePaths: false,
		consoleParsers: [[parserName: 'Java Compiler (Eclipse)']],
		excludePattern: '', healthy: '', includePattern: '', messagesPattern: '', unHealthy: ''])
	step([$class: 'TasksPublisher', canComputeNew: false, excludePattern: '', healthy: '', high: 'FIXME,XXX', low: '', normal: 'TODO', pattern: '**/*.java,**/*.?rl,**/*.xtend', unHealthy: ''])
	step([$class: 'AnalysisPublisher', canComputeNew: false, healthy: '', unHealthy: ''])
	step([$class: 'JUnitResultArchiver', allowEmptyResults: true, testResults: '**/target/surefire-reports/TEST-*.xml'])
	step([$class: 'JacocoPublisher', exclusionPattern: '**/*Test*.class,org/erlide/wrangler/**/*,org/erlide/cover/**/*,org/erlide/tracing/**/*,org/incava/**/*,org/fishwife/**/*,com/ericsson/**/*,nl/kii/**/*,org/erlide/annotations/**/*,org/erlide/util/CharOperation,org/erlide/util/Util', sourcePattern: '**/src/'])
}

def archive() {
	sh 'rm -rf VSN'
	sh 'ls releng/org.erlide.site/target/repository/features/org.erlide_*.jar | xargs basename > VSN || true'
	def archive = readFile('VSN').trim().replace('.jar', '.zip')
	if(archive != '') {
	    dir('releng/org.erlide.site/target/repository') {
		    sh "zip -r ${archive} * "
		    step([$class: 'ArtifactArchiver', artifacts: archive, fingerprint: true])
	    }
	}
    if(env.BRANCH_NAME=="master") {
    	step([$class: 'ArtifactArchiver', artifacts: 'releng/org.erlide.product.site/target/products/*.zip', fingerprint: true])
    }
	return archive
}

@NonCPS
def getVersion(String archive) {
    def m = (archive =~ /org.erlide_([0-9]+\.[0-9]+\.[0-9]+)(\.(.+))?.zip/)
    return m[0]
}

def publish(def archive) {
	sh "git remote get-url origin > REPO"
	def isMainRepo = readFile('REPO').trim().contains('github.com/erlang/')

	if(!isMainRepo) {
		// only do a release if in main repo
		return
	}

    // FIXME we can't push to https git url, needs password... Jenkins Github plugin uses https...
    //return

	def v = getVersion(archive)
	def vsn = v[1]
	def ts = v[2]

	def repo
	switch (env.BRANCH_NAME) {
		case "release": repo = "beta"; break
		case "master" : repo = "releases"; break
		default: repo = "nightly"
	}
	def kind
	switch (env.BRANCH_NAME) {
		case "release": kind = "B"; break
		case "master" : kind = "R"; break
		case "pu" : kind = "A"; break
		default: kind = ""
	}
	def dest
	if(kind == "R") {
		dest = vsn
	} else {
		dest = "${vsn}_${kind}${ts}"
	}

	def output_base = "/media/www/download.erlide.org/update"
	def full_dest = "${output_base}/archive/${repo}/${dest}"
	sh "umask 002"
	sh "mkdir -p ${full_dest}"
	sh "cp -r releng/org.erlide.site/target/repository/* ${full_dest}"
	sh "chown -R :www-data ${full_dest}"

	if(kind == "R") {
		p2_add_composite(full_dest, output_base)
		generate_version_info(vsn, output_base)
	} else if(kind != "") {
		sh "rm -f ${output_base}/${repo}"
		sh "ln -s ${full_dest} ${output_base}/${repo}"
	}
}

def run_eclipse(def dir, def opts) {
	def launcher = new File("${dir}/plugins").list().grep(~/org.eclipse.equinox.launcher_.*.jar/)
	sh "java -jar ${launcher} ${opts} -verbose"
}

def p2_add_composite(def dir, def base) {
	def relpath = java.nio.file.Paths.get(base).relativize(java.nio.file.Paths.get(dir)).toString()
	sh "chmod u+x releng/org.erlide.releng/comp-repo.sh && releng/org.erlide.releng/comp-repo.sh ${base} --eclipse ${env.HOME}/erlide_tools/buckminster/ add ${relpath}"
}

def generate_version_info(def vsn, def base) {
	sh 'git describe --tags > GIT_INFO'
	def info = readFile('GIT_INFO').trim()
	writeFile file: "${base}/info.js", text: "document.write('${info}');"
	writeFile file: "${base}/version.js", text: "document.write('${vsn}');"
	writeFile file: "${base}/id.js", text: "document.write('${env.BUILD_ID}');"
	writeFile file: "${base}/version.txt", text: "${vsn}"
}

def publishRelease(def archive) {
	def isMaster = (env.BRANCH_NAME=='master')
	sh "git remote get-url origin > REPO"
	def isMainRepo = readFile('REPO').trim().contains('github.com/erlang/')
	if(!isMaster || !isMainRepo) {
		// only do a github release if on master and in main repo
		return
	}

	def v = getVersion(archive)
	def vsn = v[1]
	def ts = v[2]
	def vvsn = "v${vsn}"
    //sh "git push origin :refs/tags/${vvsn} || true"
    sh "git fetch --prune origin +refs/tags/*:refs/tags/*"

    sh 'rm -rf GIT_TAG'
	sh 'git describe --exact-match > GIT_TAG || true'
	def git_tag = readFile('GIT_TAG').trim()
	if(git_tag != vvsn) {
		// if there is a tag, but it's not $vvsn, skip publishing
		return
	}
	if(git_tag == null || git_tag == '') {
		sh "git tag -a ${vvsn} -m ${vvsn}"
		//sh "git push origin ${vvsn}"
		//git_tag = vvsn
	}

	def draft = true
	def body = ""
	def owner = "erlang"
	def repository = "erlide_eclipse"
	def access_token = "${env.GITHUB_TOKEN}"

	sh "rm -rf RELEASE"
	def API_create="{\"tag_name\": \"${vvsn}\",\"name\": \"${vvsn}\",\"body\": \"${body}\",\"draft\": ${draft},\"prerelease\": false}"
	sh "curl -H \"Content-Type:application/json\" --data '${API_create}' https://api.github.com/repos/${owner}/${repository}/releases?access_token=${access_token} > RELEASE"
	def release = readFile('RELEASE').trim()
	def info = getReleaseInfo(release)
	def release_id = info[1]
	sh "curl -X POST --header \"Content-Type:application/edn\" --data-binary @releng/org.erlide.site/target/repository/${archive} https://uploads.github.com/repos/${owner}/${repository}/releases/${release_id}/assets?access_token=${access_token}\\&name=${archive}"

	// publish help to github.io
	val dest = "plugins/org.erlide.help/target/erlide.github.io"
	sh "rm -rf ${dest} && mkdir -p ${dest}"
	sh "git clone --depth 1 git@github.com:erlide/erlide.github.io -b master ${dest}"
	sh "cp -R plugins/org.erlide.help/articles/* ${dest}/articles/eclipse"
	dir(dest) {
		sh "git add . && git commit -a -m 'autoupdate eclipse docs (${vsn})' && git push origin master"
	}
}

@NonCPS
def getReleaseInfo(String data) {
    def m = (data.replaceAll("\n"," ").trim() =~ /\{[^{]*"id": *([^,]*),.*/)
    return m[0]
}

def getRepoURL() {
  sh "git config --get remote.origin.url > .git/remote-url"
  return readFile(".git/remote-url").trim()
}
 
def getCommitSha() {
  sh "git rev-parse HEAD > .git/current-commit"
  return readFile(".git/current-commit").trim()
}
 
def updateGithubCommitStatus(build) {
  // workaround https://issues.jenkins-ci.org/browse/JENKINS-38674
  repoUrl = getRepoURL()
  commitSha = getCommitSha()
 
  step([
    $class: 'GitHubCommitStatusSetter',
    reposSource: [$class: "ManuallyEnteredRepositorySource", url: repoUrl],
    commitShaSource: [$class: "ManuallyEnteredShaSource", sha: commitSha],
    errorHandlers: [[$class: 'ShallowAnyErrorHandler']],
    statusResultSource: [
      $class: 'ConditionalStatusResultSource',
      results: [
        [$class: 'BetterThanOrEqualBuildResult', result: 'SUCCESS', state: 'SUCCESS', message: build.description],
        [$class: 'BetterThanOrEqualBuildResult', result: 'FAILURE', state: 'FAILURE', message: build.description],
        [$class: 'AnyBuildResult', state: 'FAILURE', message: 'Loophole']
      ]
    ]
  ])
}

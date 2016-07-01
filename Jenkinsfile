#!groovy

stage 'Checkout'
node {
	wrap([$class: 'TimestamperBuildWrapper']) {
		checkout()
	}
}

stage 'Compile'
node {
	wrap([$class: 'TimestamperBuildWrapper']) {
		compile()
	}
}

//stage 'Tests'
//	runTests()

stage 'Analyze'
node {
	wrap([$class: 'TimestamperBuildWrapper']) {
		analyze()
	}
}

stage 'Archive'
node {
	wrap([$class: 'TimestamperBuildWrapper']) {
		archive = archive()
	}
}

stage 'Publish'
node {
	wrap([$class: 'TimestamperBuildWrapper']) {
		publish(archive)
		publishRelease(archive)
	}
}

///////////////////////////////////

def checkout() {
	deleteDir()
	if(env.BRANCH_NAME != null) { // multi branch
		checkout scm
		git_branch = env.BRANCH_NAME
	} else {
        git url: 'git@github.com:vladdu/erlide_eclipse.git', branch: 'pu'
	    sh 'git symbolic-ref --short HEAD > GIT_BRANCH'
    	git_branch=readFile('GIT_BRANCH').trim()
	}
    sh('git rev-parse HEAD > GIT_COMMIT')
    git_commit=readFile('GIT_COMMIT')
    short_commit=git_commit.take(6)

	//currentBuild.setName("${short_commit}__${env.BUILD_NUMBER}")
	currentBuild.setDescription("${git_branch} - ${short_commit}")
}

def compile() {
	wrap([$class: 'Xvfb', displayNameOffset: 100, installationName: 'xvfb', screen: '1024x768x24']) {
		dir('org.erlide.parent') {
			sh "chmod u+x mvnw"
			sh "./mvnw -B -U clean verify -P help -Dmaven.test.failure.ignore=true"
		}
	}
}

def analyze() {
	step([$class: 'WarningsPublisher', canComputeNew: false, canResolveRelativePaths: false,
		consoleParsers: [[parserName: 'Java Compiler (Eclipse)']],
		excludePattern: '', healthy: '', includePattern: '', messagesPattern: '', unHealthy: ''])

	step([$class: 'TasksPublisher', canComputeNew: false, excludePattern: '', healthy: '', high: 'FIXME,XXX', low: '', normal: 'TODO', pattern: '**/*.java,**/*.?rl,**/*.xtend', unHealthy: ''])

	step([$class: 'AnalysisPublisher', canComputeNew: false, healthy: '', unHealthy: ''])

	step([$class: 'JUnitResultArchiver', allowEmptyResults: true, testResults: '**/target/surefire-reports/TEST-*.xml'])

	// locks

	// jacoco

}

def archive() {
	sh 'rm -rf VSN'
	sh 'ls org.erlide.site/target/repository/features/org.erlide_*.jar | xargs basename > VSN || true'
	def archive = readFile('VSN').trim().replace('.jar', '.zip')
	if(archive != '') {
	    dir('org.erlide.site/target/repository') {
		    sh "zip -r ${archive} * "
		    step([$class: 'ArtifactArchiver', artifacts: archive, fingerprint: true])
	    }
	}
	return archive
}

@NonCPS
def getVersion(String archive) {
    def m = (archive =~ /org.erlide_([0-9]+\.[0-9]+\.[0-9]+)\.(.+).zip/)
    return m[0]
}

def publish(def archive) {
	def v = getVersion(archive)
	def vsn = v[1]
	def ts = v[2]

	def repo
	switch (git_branch) {
		case "release": repo = "beta"; break
		case "master" : repo = "releases"; break
		default: repo = "nightly"
	}
	def kind
	switch (git_branch) {
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
	sh "cp -r org.erlide.site/target/repository/* ${full_dest}"
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
	sh "chmod u+x org.erlide.releng/comp-repo.sh && org.erlide.releng/comp-repo.sh ${base} --eclipse ${env.HOME}/erlide_tools/buckminster/ add ${relpath}"
}

def generate_version_info(def vsn, def base) {
	sh 'git describe > GIT_INFO'
	def info = readFile('GIT_INFO').trim()
	writeFile file: "${base}/info.js", text: "document.write('${info}');"
	writeFile file: "${base}/version.js", text: "document.write('${vsn}');"
	writeFile file: "${base}/id.js", text: "document.write('${env.BUILD_ID}');"
	writeFile file: "${base}/version.txt", text: "${vsn}"
}

def publishRelease(def archive) {
	def isMaster = (git_branch=='master')
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
    sh "git push origin :refs/tags/${vvsn}"
    sh "git fetch --prune origin +refs/tags/*:refs/tags/*"

    sh 'rm -rf GIT_TAG'
	sh 'git describe --exact-match > GIT_TAG || true'
	def git_tag = readFile('GIT_TAG').trim()
	if(git_tag == null || git_tag == '') {
		sh "git tag -a ${vvsn} -m ${vvsn}"
		sh "git push origin ${vvsn}"
		git_tag = vvsn
	}
	if(git_tag != vvsn) {
		// if there is a tag, but it's not $vvsn, skip publishing
		return
	}

	def draft = true
	def body = "test"
	def owner = "vladdu" // "erlang"
	def repository = "erlide_eclipse"
	def access_token = "${env.GITHUB_TOKEN}"

	sh "rm -rf RELEASE"
	def API_create="{\"tag_name\": \"${vvsn}\",\"name\": \"${vvsn}\",\"body\": \"${body}\",\"draft\": ${draft},\"prerelease\": false}"
	sh "curl -H \"Content-Type:application/json\" --data '${API_create}' https://api.github.com/repos/${owner}/${repository}/releases?access_token=${access_token} > RELEASE"
	def release = readFile('RELEASE').trim()
	def info = getReleaseInfo(release)
	def release_id = info[1]
	sh "curl -X POST --header \"Content-Type:application/edn\" --data-binary @org.erlide.site/target/repository/${archive} https://uploads.github.com/repos/${owner}/${repository}/releases/${release_id}/assets?access_token=${access_token}\\&name=${archive}"
}

@NonCPS
def getReleaseInfo(String data) {
    def m = (data.replaceAll("\n"," ").trim() =~ /\{[^{]*"id": *([^,]*),.*/)
    return m[0]
}


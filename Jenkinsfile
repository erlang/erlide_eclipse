node {
	wrap([$class: 'TimestamperBuildWrapper']) {
		stage 'Checkout'
			checkout scm
			//currentBuild.setName("${env.GIT_REVISION}__${env.BUILD_NUMBER}") 
			currentBuild.setDescription("${env.GIT_BRANCH}") 
			echo "1: ${env.BUILD_TAG}"
			sh "env"

		stage 'Compile'
			wrap([$class: 'Xvfb', additionalOptions: '', assignedLabels: '', displayNameOffset: 1, installationName: 'xvfb', screen: '1024x768x24']) {
				dir('org.erlide.parent') {
					sh "./mvnw -B -U clean verify -P help -Dmaven.test.failure.ignore=true"
				}
			}

			step([$class: 'WarningsPublisher', canComputeNew: false, canResolveRelativePaths: false, consoleParsers: [[parserName: 'Maven']], defaultEncoding: '', excludePattern: '', healthy: '', includePattern: '', messagesPattern: '', unHealthy: ''])

			step([$class: 'TasksPublisher', canComputeNew: false, defaultEncoding: '', excludePattern: '', healthy: '', high: 'FIXME,XXX', low: '', normal: 'TODO', pattern: '**/*.java,**/*.?rl,**/*.xtend', unHealthy: ''])

			step([$class: 'AnalysisPublisher', canComputeNew: false, defaultEncoding: '', healthy: '', unHealthy: ''])

			step([$class: 'JUnitResultArchiver', testResults: '**/target/surefire-reports/TEST-*.xml'])

			// locks

			// jacoco

		stage 'Publish'
			step([$class: 'ArtifactArchiver', artifacts: '**/target/repository/**/*.*', fingerprint: true])
	}
}


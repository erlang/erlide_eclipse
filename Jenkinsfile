node {
	wrap([$class: 'TimestamperBuildWrapper']) {
		stage 'Checkout'
			//checkout scm
			//currentBuild.setName("${env.GIT_REVISION}__${env.BUILD_NUMBER}") 

		stage 'Compile'
			wrap([$class: 'Xvfb', additionalOptions: '', assignedLabels: '', displayNameOffset: 1, installationName: 'xvfb', screen: '1024x768x24']) {
				sh "bundle install"
				sh "rake -f org.erlide.releng/Rakefile build_help run_tests"
			}

		step 'Analyze'
			step([$class: 'WarningsPublisher', canComputeNew: false, canResolveRelativePaths: false, consoleParsers: [[parserName: 'Buckminster']], defaultEncoding: '', excludePattern: '', healthy: '', includePattern: '', messagesPattern: '', unHealthy: ''])

			step([$class: 'TasksPublisher', canComputeNew: false, defaultEncoding: '', excludePattern: '', healthy: '', high: 'FIXME,XXX', low: '', normal: 'TODO', pattern: '**/*.java,**/*.?rl,**/*.xtend', unHealthy: ''])

			step([$class: 'AnalysisPublisher', canComputeNew: false, defaultEncoding: '', healthy: '', unHealthy: ''])

			step([$class: 'JUnitResultArchiver', testResults: '**/junit.xml,**/.eunit/*.xml'])

			// locks

			// jacoco

		stage 'Publish'
			//step([$class: 'ArtifactArchiver', artifacts: 'buildroot/**/*.*', fingerprint: true])
	}
}


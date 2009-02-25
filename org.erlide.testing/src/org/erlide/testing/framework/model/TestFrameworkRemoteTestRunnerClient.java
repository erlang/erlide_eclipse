package org.erlide.testing.framework.model;


public class TestFrameworkRemoteTestRunnerClient {

	private TestFrameworkTestRunSession fTestRunSession;

	public TestFrameworkRemoteTestRunnerClient(TestFrameworkTestRunSession testRunSession) {
		fTestRunSession = testRunSession;
		new Thread() {
			@Override
			public void run() {
				try {

					long now = System.currentTimeMillis();

					Thread.sleep(1000);
					fTestRunSession.testRunStarted(4);

					System.out.println("starting tests...");

					fTestRunSession.testTreeEntry("0,testSuite1,-");
					fTestRunSession.testTreeEntry("1,testCase1_1,0");
					fTestRunSession.testTreeEntry("2,testCase1_2,0");
					fTestRunSession.testTreeEntry("3,testCase1_3,0");
					fTestRunSession.testTreeEntry("4,testSuite2,-");
					fTestRunSession.testTreeEntry("5,testCase2_1,4");
					Thread.sleep(2000);

					// fTestRunSession.testStarted("0");
					// fTestRunSession.testStarted("1");
					// Thread.sleep(2000);
					// fTestRunSession.testEnded("1");
					// fTestRunSession.testStarted("2");
					// Thread.sleep(2000);
					// fTestRunSession.testFailed("2", "", "", "");
					// fTestRunSession.testEnded("2");
					// fTestRunSession.testStarted("3");
					// Thread.sleep(2000);
					// fTestRunSession.testEnded("3");
					// fTestRunSession.testEnded("0");
					//
					// fTestRunSession.testStarted("4");
					// fTestRunSession.testStarted("5");
					// Thread.sleep(2000);
					// fTestRunSession.testEnded("5");
					// fTestRunSession.testEnded("4");

					fTestRunSession.testRunEnded(System.currentTimeMillis()
							- now);

				} catch (InterruptedException e) {
					e.printStackTrace();
				}

			}
		}.start();
	}

}

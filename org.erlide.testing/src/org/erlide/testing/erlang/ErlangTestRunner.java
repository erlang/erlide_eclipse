package org.erlide.testing.erlang;

import org.junit.runner.Description;
import org.junit.runner.Runner;
import org.junit.runner.notification.RunNotifier;

public class ErlangTestRunner extends Runner {

	private Class<? extends ErlangTestSuite> clazz;
	private ErlangTestSuite suite;

	public ErlangTestRunner(Class<? extends ErlangTestSuite> testClass) {
		clazz = testClass;
		try {
			suite = clazz.newInstance();
		} catch (InstantiationException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		}
	}

	@Override
	public Description getDescription() {
		Description description = Description
				.createSuiteDescription("erl test 123");
		description.addChild(Description.createTestDescription(clazz, clazz
				.getSimpleName()));
		return description;
	}

	@Override
	public void run(RunNotifier notifier) {
	}

}

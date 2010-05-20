package org.erlide.wrangler.refactoring.ui;

import org.eclipse.core.expressions.PropertyTester;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

public class QuickCheckTester extends PropertyTester {

	public QuickCheckTester() {
	}

	public boolean test(Object receiver, String property, Object[] args,
			Object expectedValue) {
		if (property.equals("hasQuickCheck")) {
			return GlobalParameters.hasQuickCheck();
		}
		return true;
	}

}

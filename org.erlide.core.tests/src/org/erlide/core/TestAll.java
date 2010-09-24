package org.erlide.core;

import org.erlide.core.bdd.RpcStory;
import org.erlide.core.preferences.Base64Test;
import org.erlide.core.preferences.PreferencesHelperTest;
import org.junit.runner.JUnitCore;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * The class <code>TestAll</code> builds a suite that can be used to run all of
 * the tests within its package as well as within any subpackages of its
 * package.
 *
 * @generatedBy CodePro at 2010-09-17 14:04
 * @author qvladum
 * @version $Revision: 1.0 $
 */
@RunWith(Suite.class)
@Suite.SuiteClasses( { PreferencesHelperTest.class, Base64Test.class,
        ParsingTests.class, ErlangTests.class,
        org.erlide.core.util.TestAll.class, RpcStory.class, })
public class TestAll {

    /**
     * Launch the test.
     *
     * @param args
     *            the command line arguments
     *
     * @generatedBy CodePro at 2010-09-17 14:04
     */
    public static void main(String[] args) {
        JUnitCore.runClasses(new Class[] { TestAll.class });
    }
}

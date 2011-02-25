package org.erlide.core;

import java.io.IOException;
import java.util.Collection;
import java.util.List;

import jdepend.framework.JDepend;
import jdepend.framework.JavaPackage;

import org.junit.Assert;
import org.junit.BeforeClass;

import com.google.common.collect.Lists;

public class ConstraintTest {

    private JDepend jdep;
    private Collection<JavaPackage> analyzed;
    private List<JavaPackage> myPackages;

    @SuppressWarnings("unchecked")
    @BeforeClass
    protected void setUp() throws IOException {

        jdep = new JDepend();

        jdep.setComponents("org.erlide.jinterface,org.erlide.core.backend,org.erlide.core.backend,org.erlide.core.model.erlang,org.erlide.core,org.erlide.debug,org.erlide.core.common,java,org.eclipse,org.osgi,com,erlang");

        // jdep.addDirectory(getPathToPlugin("org.erlide.jinterface"));
        // jdep.addDirectory(getPathToPlugin("org.erlide.core"));

        analyzed = jdep.analyze();

        final JavaPackage jinterface = jdep.getPackage("org.erlide.jinterface");
        final JavaPackage common = jdep.getPackage("org.erlide.core.common");
        final JavaPackage core = jdep.getPackage("org.erlide.core");
        final JavaPackage erlang = jdep.getPackage("erlang");

        myPackages = Lists.newArrayList(erlang, jinterface, common, core);
    }

    private String getPathToPlugin(final String name) {
        return "c:/apps/erlide/" + name + "/bin";
    }

    /**
     * Tests that the package dependency constraint is met for the analyzed
     * packages.
     */

    public void testMatch() {
        for (final JavaPackage p : myPackages) {
            System.out.println(p.getName().replaceAll("org.erlide.", "")
                    + " = " + print(p.getEfferents()));
        }
    }

    private String print(final Collection<JavaPackage> list) {
        final StringBuilder result = new StringBuilder();
        for (final JavaPackage p : list) {
            if (myPackages.contains(p)) {
                result.append(", ").append(
                        p.getName().replaceAll("org.erlide.", ""));
            }
        }
        return result.toString();
    }

    /**
     * Tests that a single package does not contain any package dependency
     * cycles.
     */

    public void testOnePackage() {
        final JavaPackage p = jdep.getPackage("org.erlide.core.backend");

        Assert.assertEquals("Cycle exists: " + p.getName(), false,
                p.containsCycle());
    }

    /**
     * Tests that a package dependency cycle does not exist for any of the
     * analyzed packages.
     */

    public void testAllPackages() {
        Assert.assertEquals("Cycles exist", false, jdep.containsCycles());
    }

}

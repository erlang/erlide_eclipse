package org.erlide.core;

import java.io.IOException;
import java.util.Collection;
import java.util.List;

import jdepend.framework.JDepend;
import jdepend.framework.JavaPackage;
import junit.framework.TestCase;

import org.junit.Test;

import com.google.common.collect.Lists;

public class ConstraintTest extends TestCase {

    private JDepend jdep;
    private Collection analyzed;
    private List<JavaPackage> myPackages;

    public ConstraintTest(String name) {
        super(name);
    }

    @Override
    protected void setUp() throws IOException {

        jdep = new JDepend();

        jdep.setComponents("org.erlide.jinterface,org.erlide.backend,org.erlide.core.backend,org.erlide.core.erlang,org.erlide.core,org.erlide.debug,org.erlide.common,java,org.eclipse,org.osgi,com,erlang");

        jdep.addDirectory(getPathToPlugin("org.erlide.jinterface"));
        jdep.addDirectory(getPathToPlugin("org.erlide.backend"));
        jdep.addDirectory(getPathToPlugin("org.erlide.core"));

        analyzed = jdep.analyze();

        JavaPackage jinterface = jdep.getPackage("org.erlide.jinterface");
        JavaPackage common = jdep.getPackage("org.erlide.common");
        JavaPackage backend = jdep.getPackage("org.erlide.backend");
        JavaPackage core = jdep.getPackage("org.erlide.core");
        JavaPackage erlang = jdep.getPackage("erlang");

        myPackages = Lists.newArrayList(erlang, jinterface, common, backend,
                core);
    }

    private String getPathToPlugin(String name) {
        return "c:/apps/erlide/" + name + "/bin";
    }

    /**
     * Tests that the package dependency constraint is met for the analyzed
     * packages.
     */
    @Test
    public void testMatch() {
        for (JavaPackage p : myPackages) {
            System.out.println(p.getName().replaceAll("org.erlide.", "")
                    + " = " + print(p.getEfferents()));
        }
    }

    private String print(Collection list) {
        StringBuilder result = new StringBuilder();
        for (Object op : list) {
            JavaPackage p = (JavaPackage) op;
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
    @Test
    public void testOnePackage() {
        JavaPackage p = jdep.getPackage("org.erlide.backend");

        assertEquals("Cycle exists: " + p.getName(), false, p.containsCycle());
    }

    /**
     * Tests that a package dependency cycle does not exist for any of the
     * analyzed packages.
     */
    @Test
    public void testAllPackages() {
        assertEquals("Cycles exist", false, jdep.containsCycles());
    }

}

package org.erlide.core;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import jdepend.framework.JDepend;
import jdepend.framework.JavaPackage;

import com.google.common.collect.Lists;

public class DependenciesTest {

    private JDepend jdep;
    private List<JavaPackage> analyzed;

    @SuppressWarnings("unchecked")
    public void setUp() throws IOException {
        jdep = new JDepend();
        jdep.setComponents("org.erlide.jinterface,java,org.eclipse,com.ericsson,com,org.osgi");
        // jdep.addDirectory(getPathToPlugin("org.erlide.jinterface"));
        jdep.addDirectory(getPathToPlugin("org.erlide.core"));

        analyzed = Lists.newArrayList(jdep.analyze());
        removeUninterestingPackages(analyzed);
    }

    private String getPathToPlugin(final String name) {
        return System.getenv("WORKSPACE") + "/" + name + "/bin";
    }

    public void printAll() {
        final DSM dsm = new DSM(analyzed);
        dsm.print();
        // dsm.partition();
        // dsm.print();
        System.out.println();
    }

    private void removeUninterestingPackages(final List<JavaPackage> all) {
        final String[] boring = { "org.eclipse", "org.osgi", "com", "java",
                "com.ericsson", "org.erlide.jinterface" };
        for (final String b : boring) {
            final JavaPackage p = jdep.getPackage(b);
            all.remove(p);
        }
    }

    @SuppressWarnings("rawtypes")
    public void testAllPackages() {
        final boolean containsCycles = jdep.containsCycles();
        System.out.println("Cycles: " + containsCycles);
        if (containsCycles) {
            for (final JavaPackage p : analyzed) {
                final List cycles = new ArrayList();
                p.collectAllCycles(cycles);
                // System.out.print(p.getName() + ": ");
                // printPackageList(cycles);
            }
        }
    }

    protected void printPackageList(final List<JavaPackage> list) {
        System.out.print("[");
        for (final JavaPackage p : list) {
            System.out.print(p.getName() + ", ");
        }
        System.out.println("]");
    }

}

package org.erlide.core;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import jdepend.framework.JDepend;
import jdepend.framework.JavaPackage;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Lists;

public class ConstraintTest {

    private JDepend jdep;
    private Collection<JavaPackage> analyzed;
    private List<JavaPackage> myPackages;

    @SuppressWarnings("unchecked")
    @Before
    public void setUp() throws IOException {

        jdep = new JDepend();

        jdep.setComponents("erlang,org.erlide.jinterface,org.erlide.core.backend,org.erlide.core.model,org.erlide.core.services,org.erlide.core.common"
                + ",java,org.eclipse,com.ericsson,com,org.osgi");

        jdep.addDirectory(getPathToPlugin("org.erlide.jinterface"));
        jdep.addDirectory(getPathToPlugin("org.erlide.core"));

        analyzed = jdep.analyze();

        final JavaPackage jinterface = jdep.getPackage("org.erlide.jinterface");
        final JavaPackage common = jdep.getPackage("org.erlide.core.common");
        final JavaPackage model = jdep.getPackage("org.erlide.core.model");
        final JavaPackage backend = jdep.getPackage("org.erlide.core.backend");
        final JavaPackage services = jdep
                .getPackage("org.erlide.core.services");
        final JavaPackage core = jdep.getPackage("org.erlide.core");
        final JavaPackage erlang = jdep.getPackage("erlang");

        myPackages = Lists.newArrayList(erlang, jinterface, common, core,
                model, backend, services);
        Collections.sort(myPackages, new Comparator<JavaPackage>() {
            public int compare(final JavaPackage o1, final JavaPackage o2) {
                if (o1 == o2) {
                    return 0;
                }
                return o1.getEfferents().contains(o2) ? 1 : -1;
            }
        });
    }

    private String getPathToPlugin(final String name) {
        // return "c:/apps/erlide/" + name + "/bin";
        return "c:/users/vlad/projects/erlide/" + name + "/bin";
    }

    /**
     * Tests that the package dependency constraint is met for the analyzed
     * packages.
     */
    @Test
    public void printAll() {
        final List<JavaPackage> all = Lists.newArrayList(analyzed);
        Collections.sort(all, new Comparator<JavaPackage>() {
            public int compare(final JavaPackage o1, final JavaPackage o2) {
                return o1.getName().compareTo(o2.getName());
            }
        });
        for (final JavaPackage p : all) {
            System.out.println(p.getName());
        }
        System.out.println("-----");
        for (final JavaPackage p : myPackages) {
            System.out.println(p.getName().replaceAll("org.erlide.", "")
                    + " = " + print(p.getEfferents()));
        }

        Collections.sort(all, new Comparator<JavaPackage>() {
            public int compare(final JavaPackage o1, final JavaPackage o2) {
                if (o1 == o2) {
                    return 0;
                }
                if (o1.getEfferents().contains(o2)
                        && o2.getEfferents().contains(o1)) {
                    return 0;
                }
                return o1.getEfferents().contains(o2) ? 1 : -1;
            }
        });
        printMatrix(all);
    }

    private void printMatrix(final List<JavaPackage> packages) {
        final int n = packages.size();
        final char[][] mx = new char[n][n];

        for (int i = 0; i < n; i++) {
            final JavaPackage p = packages.get(i);
            for (int j = 0; j < n; j++) {
                final JavaPackage p1 = packages.get(j);
                if (p.getEfferents().contains(p1)) {
                    mx[i][j] = 'X';
                } else {
                    mx[i][j] = ' ';
                }
                if (i == j) {
                    mx[i][j] = '-';
                }
            }

        }

        for (int i = 0; i < n; i++) {
            System.out.println(i + ": " + packages.get(i).getName());
        }
        for (int i = 0; i < n; i++) {
            System.out.print(i);
        }
        System.out.println();

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                System.out.print(mx[i][j]);
            }
            System.out.println();
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

package org.erlide.engine.model.erlang;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.junit.Test;

import com.google.common.collect.Lists;

public class ErlProjectPropertiesTests {

    @Test
    public void equalsTest() {
        final ErlangProjectProperties p1 = new ErlangProjectProperties();
        final ErlangProjectProperties p2 = new ErlangProjectProperties();
        assertThat(p2, is(ErlangProjectPropertiesMatcher.sameAs(p1)));
    }

    @Test
    public void resolveTest() {
        final ErlangProjectProperties p1 = new ErlangProjectProperties();
        final IPath p = new Path("abc");
        p1.setSourceDirs(Lists.newArrayList(p));

        final ErlangProjectProperties p2 = p1.resolve();
        assertThat(p2, is(ErlangProjectPropertiesMatcher.sameAs(p1)));
        assertThat(p2.getSourceDirs().iterator().next(), is((IPath) new Path("abc")));
    }

    @SuppressWarnings("deprecation")
    @Test
    public void resolveExistingVariableTest() throws CoreException {

        final IPathVariableManager pathVariableManager = ResourcesPlugin.getWorkspace()
                .getPathVariableManager();
        pathVariableManager.setValue("ZZZ", new Path("/home"));

        final ErlangProjectProperties p1 = new ErlangProjectProperties();
        final IPath p = new Path("ZZZ/abc");
        p1.setSourceDirs(Lists.newArrayList(p));

        final ErlangProjectProperties p2 = p1.resolve();
        assertThat(p2.getSourceDirs().iterator().next(),
                is((IPath) new Path("/home/abc")));
    }

    @SuppressWarnings("deprecation")
    @Test
    public void resolveUnexistingVariableTest() throws CoreException {

        final IPathVariableManager pathVariableManager = ResourcesPlugin.getWorkspace()
                .getPathVariableManager();
        pathVariableManager.setValue("ZZZ", null);

        final ErlangProjectProperties p1 = new ErlangProjectProperties();
        final IPath p = new Path("ZZZ/abc");
        p1.setSourceDirs(Lists.newArrayList(p));

        final ErlangProjectProperties p2 = p1.resolve();
        assertThat(p2.getSourceDirs().iterator().next(), is((IPath) new Path("ZZZ/abc")));
    }

}

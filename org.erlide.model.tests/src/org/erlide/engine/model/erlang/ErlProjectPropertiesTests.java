package org.erlide.engine.model.erlang;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.junit.Test;

public class ErlProjectPropertiesTests {

    @Test
    public void defaultValuesTest() {
        final ErlangProjectProperties p = new ErlangProjectProperties();
        assertThat(p.getOutputDir(), is((IPath) new Path("")));
        assertThat(p.getSourceDirs(), is(empty()));
        assertThat(p.getIncludeDirs(), is(empty()));
    }

    @Test
    public void equalsTest() {
        final ErlangProjectProperties p1 = new ErlangProjectProperties();
        final ErlangProjectProperties p2 = new ErlangProjectProperties();
        assertThat(p2, is(ErlangProjectPropertiesMatcher.sameAs(p1)));
    }

}

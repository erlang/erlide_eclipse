package org.erlide.engine.model.erlang;

import static com.google.common.truth.Truth.assertThat;

import org.eclipse.core.runtime.Path;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.junit.Test;

public class ErlProjectPropertiesTest {

    @Test
    public void defaultValuesTest() {
        final ErlangProjectProperties p = new ErlangProjectProperties();
        assertThat(p.getOutputDir()).isEqualTo(new Path(""));
        assertThat(p.getSourceDirs()).isEmpty();
        assertThat(p.getIncludeDirs()).isEmpty();
    }

    @Test
    public void equalsTest() {
        final ErlangProjectProperties p1 = new ErlangProjectProperties();
        final ErlangProjectProperties p2 = new ErlangProjectProperties();
        assertThat(p2).isEqualTo(p1);
    }

}

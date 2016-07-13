package org.erlide.engine.services.codeassist;

import static com.google.common.truth.Truth.assertThat;

import java.util.Collection;

import org.erlide.engine.ErlangEngine;
import org.junit.Test;

import com.google.common.collect.Sets;

public class CodeAssistTest {

    @Test
    public void getVariablesShouldReturnNonNull() {
        final Collection<String> result = ErlangEngine.getInstance()
                .getContextAssistService().getVariables("src", "");
        assertThat(result).isNotNull();
    }

    @Test
    public void getVariables_1() {
        final Collection<String> result = ErlangEngine.getInstance()
                .getContextAssistService()
                .getVariables("a(X)-> XY=-X, {Z, W}=XY, X.", "");
        final Collection<String> expected = Sets.newHashSet("X", "XY", "Z", "W");
        assertThat(result).isEqualTo(expected);
    }

    @Test
    public void getVariables_2() {
        final Collection<String> result = ErlangEngine.getInstance()
                .getContextAssistService().getVariables("a(X)-> XY=-X, Z=XY, X.", "X");
        final Collection<String> expected = Sets.newHashSet("X", "XY");
        assertThat(result).isEqualTo(expected);
    }
}

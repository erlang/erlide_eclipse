package org.erlide.core.services.codeassist;

import java.util.Collection;

import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Test;

import com.google.common.collect.Sets;

public class CodeAssistTest {

    @Test
    public void getVariablesShouldReturnNonNull() {
        final IBackend b = BackendCore.getBackendManager().getIdeBackend();
        final Collection<String> result = ErlideContextAssist.getVariables(b,
                "src", "");
        MatcherAssert.assertThat(result, Matchers.notNullValue());
    }

    @Test
    public void getVariables_1() {
        final IBackend b = BackendCore.getBackendManager().getIdeBackend();
        final Collection<String> result = ErlideContextAssist.getVariables(b,
                "a(X)-> XY=-X, {Z, W}=XY, X.", "");
        final Collection<String> expected = Sets
                .newHashSet("X", "XY", "Z", "W");
        MatcherAssert.assertThat(result, Matchers.is(expected));
    }

    @Test
    public void getVariables_2() {
        final IBackend b = BackendCore.getBackendManager().getIdeBackend();
        final Collection<String> result = ErlideContextAssist.getVariables(b,
                "a(X)-> XY=-X, Z=XY, X.", "X");
        final Collection<String> expected = Sets.newHashSet("X", "XY");
        MatcherAssert.assertThat(result, Matchers.is(expected));
    }
}

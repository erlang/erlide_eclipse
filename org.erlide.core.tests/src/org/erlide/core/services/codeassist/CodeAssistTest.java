package org.erlide.core.services.codeassist;

import java.util.Collection;

import org.erlide.core.backend.BackendCore;
import org.erlide.core.rpc.IRpcCallSite;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Test;

public class CodeAssistTest {

    @Test
    public void shouldreturnNonNull() {
        final IRpcCallSite b = BackendCore.getBackendManager().getIdeBackend();
        final Collection<String> result = ErlideContextAssist.getVariables(b,
                "src", "");
        MatcherAssert.assertThat(result, Matchers.notNullValue());
    }

}

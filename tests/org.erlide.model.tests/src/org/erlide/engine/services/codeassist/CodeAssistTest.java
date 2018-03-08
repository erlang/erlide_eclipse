package org.erlide.engine.services.codeassist;

import static com.google.common.truth.Truth.assertThat;

import java.util.Collection;

import org.erlide.engine.internal.services.codeassist.ErlideContextAssist;
import org.erlide.engine.model.OtpRpcFactory;
import org.erlide.runtime.rpc.IOtpRpc;
import org.junit.BeforeClass;
import org.junit.Test;

import com.google.common.collect.Sets;

public class CodeAssistTest {

    private static ErlideContextAssist assist;

    @BeforeClass
    public static void before() {
        final IOtpRpc backend = OtpRpcFactory.getOtpRpc();
        CodeAssistTest.assist = new ErlideContextAssist(backend);
    }

    @Test
    public void getVariablesShouldReturnNonNull() {
        final Collection<String> result = CodeAssistTest.assist.getVariables("src", "");
        assertThat(result).isNotNull();
    }

    @Test
    public void getVariables_1() {
        final Collection<String> result = CodeAssistTest.assist
                .getVariables("a(X)-> XY=-X, {Z, W}=XY, X.", "");
        final Collection<String> expected = Sets.newHashSet("X", "XY", "Z", "W");
        assertThat(result).isEqualTo(expected);
    }

    @Test
    public void getVariables_2() {
        final Collection<String> result = CodeAssistTest.assist.getVariables("a(X)-> XY=-X, Z=XY, X.",
                "X");
        final Collection<String> expected = Sets.newHashSet("X", "XY");
        assertThat(result).isEqualTo(expected);
    }
}

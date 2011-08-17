package org.erlide.core.bdd;

import static org.hamcrest.MatcherAssert.assertThat;

import org.erlide.core.backend.BackendCore;
import org.erlide.core.rpc.IRpcCallSite;
import org.erlide.core.rpc.RpcException;
import org.erlide.jinterface.util.TermParser;
import org.erlide.jinterface.util.TermParserException;
import org.jbehave.core.annotations.Given;
import org.jbehave.core.annotations.Then;
import org.jbehave.core.annotations.When;

import com.ericsson.otp.erlang.OtpErlangObject;

public class RpcSteps {

    private IRpcCallSite backend;
    private OtpErlangObject result = null;
    final private TermParser termParser = TermParser.getParser();

    @Given("a backend")
    public void aBackend() {
        System.out.println("GIVEN");
        backend = BackendCore.getBackendManager().getIdeBackend();
    }

    @When("a rpc is done with args $m:$f($a)")
    public void aRpcIsDoneWith(final String m, final String f, final String a)
            throws RpcException, TermParserException {
        System.out.println("WHEN " + m + ":" + f + " " + a);
        final OtpErlangObject args = termParser.parse(a);
        final String sig = "x";
        result = backend.call(m, f, sig, args);
    }

    @Then("the result should be $value")
    public void theResultShouldBe(final String value)
            throws TermParserException {
        System.out.println("THEN " + value + " expect " + value);
        final OtpErlangObject v = termParser.parse(value);
        assertThat("ok", result.equals(v));
    }

}

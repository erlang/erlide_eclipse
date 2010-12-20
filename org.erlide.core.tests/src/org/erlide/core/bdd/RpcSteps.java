package org.erlide.core.bdd;

import static org.hamcrest.MatcherAssert.assertThat;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ParserException;
import org.erlide.jinterface.util.TermParser;
import org.erlide.runtime.backend.BackendManager;
import org.jbehave.core.annotations.Given;
import org.jbehave.core.annotations.Then;
import org.jbehave.core.annotations.When;

import com.ericsson.otp.erlang.OtpErlangObject;

public class RpcSteps {

    private Backend backend;
    private OtpErlangObject result = null;
    final private TermParser termParser = TermParser.getParser();

    @Given("a backend")
    public void aBackend() {
        System.out.println("GIVEN");
        backend = BackendManager.getDefault().getIdeBackend();
    }

    @When("a rpc is done with args $m:$f($a)")
    public void aRpcIsDoneWith(final String m, final String f, final String a)
            throws BackendException, ParserException {
        System.out.println("WHEN " + m + ":" + f + " " + a);
        final OtpErlangObject args = termParser.parse(a);
        final String sig = "x";
        result = backend.call(m, f, sig, args);
    }

    @Then("the result should be $value")
    public void theResultShouldBe(final String value) throws ParserException {
        System.out.println("THEN " + value + " expect " + value);
        final OtpErlangObject v = termParser.parse(value);
        assertThat("ok", result.equals(v));
    }

}

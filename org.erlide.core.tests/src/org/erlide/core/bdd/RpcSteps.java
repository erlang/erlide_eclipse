package org.erlide.core.bdd;

import static org.hamcrest.MatcherAssert.assertThat;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ParserException;
import org.erlide.jinterface.util.TermParser;
import org.erlide.runtime.backend.BackendManager;
import org.jbehave.scenario.annotations.Given;
import org.jbehave.scenario.annotations.Then;
import org.jbehave.scenario.annotations.When;

import com.ericsson.otp.erlang.OtpErlangObject;

public class RpcSteps {

	private Backend backend;
	private OtpErlangObject result = null;

	@Given("a backend")
	public void aBackend() {
		backend = BackendManager.getDefault().getIdeBackend();
	}

	@When("a rpc is done with args $m:$f($a)")
	public void aRpcIsDoneWith(String m, String f, String a)
			throws BackendException, ParserException {
		OtpErlangObject args = TermParser.parse(a);
		String sig = "x";
		result = backend.call(m, f, sig, args);
	}

	@Then("the result should be $value")
	public void theResultShouldBe(String value) throws ParserException {
		OtpErlangObject v = TermParser.parse(value);
		assertThat("ok", result.equals(v));
	}

}

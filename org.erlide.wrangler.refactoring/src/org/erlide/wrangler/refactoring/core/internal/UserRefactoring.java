package org.erlide.wrangler.refactoring.core.internal;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.erlide.core.rpc.IRpcResult;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.SimpleOneStepWranglerRefactoring;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Abstract class for user refactoring handlers
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public abstract class UserRefactoring extends SimpleOneStepWranglerRefactoring {

    private String callbackModule; // callback module
    private List<String> parPrompts = new LinkedList<String>(); // parameter
                                                                // prompts
    private List<String> parValues = new ArrayList<String>(0); // parameter
                                                               // values
                                                               // submited by
                                                               // user
    protected boolean fetched; // if parameter prompts are already fetched

    public String getCallbackModule() {
        return callbackModule;
    }

    /**
     * Ad hoc refactoring needs name of the callback module to be called
     * 
     * @param module
     */
    public void setCallbackModuleName(String module) {
        callbackModule = module;
        fetched = false;
    }

    /**
     * Fetch parameter prompts from the right callback module
     * 
     * @param module
     */
    public boolean fetchParPrompts() {
        if (fetched)
            return true;

        IRpcResult res = WranglerBackendManager.getRefactoringBackend()
                .callWithoutParser("input_par_prompts_eclipse", "s",
                        callbackModule);
        OtpErlangList params = (OtpErlangList) ((OtpErlangTuple) res.getValue())
                .elementAt(1);
        parPrompts.clear();
        for (OtpErlangObject obj : params.elements())
            parPrompts.add(obj.toString().replace("\"", ""));

        fetched = true;

        return true;
    }

    /**
     * Getter for parameter prompts
     * 
     * @return
     */
    public List<String> getParPrompts() {
        return parPrompts;
    }

    /**
     * Clear parameter prompts list
     */
    protected void clearParPrompts() {
        parPrompts.clear();
    }

    /**
     * Add a new prompt
     * 
     * @param prompt
     */
    protected void addParPrompts(String prompt) {
        parPrompts.add(prompt);
    }

    /**
     * Getter for parameter values
     * 
     * @return
     */
    public List<String> getParValues() {
        return parValues;
    }

    /**
     * Appends new parameter value
     * 
     * @param value
     */
    public void setParValue(List<String> params) {
        parValues = params;
    }

    protected OtpErlangList prepareUserInput() {
        OtpErlangObject[] params = new OtpErlangObject[parValues.size()];
        int i = 0;
        for (String val : parValues) {
            params[i] = new OtpErlangString(val);
            i++;
        }
        return new OtpErlangList(params);
    }

}

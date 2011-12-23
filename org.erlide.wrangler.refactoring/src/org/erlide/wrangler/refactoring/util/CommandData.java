package org.erlide.wrangler.refactoring.util;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * A class(structure) to aggregate data about each command in the composite
 * refactoring
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class CommandData {

    public String command; // command text
    public OtpErlangObject[] args; // arguments
    public String msg; // question
    public String type; // type of command (interactive, repeat interactive,
                        // non-interactive

    public void addUserInput(final List<String> input) {

        final List<OtpErlangObject> argsTmp = new LinkedList<OtpErlangObject>();
        final OtpErlangObject[] userInput = new OtpErlangObject[input.size()];

        int i = 0;
        for (final String text : input) {
            userInput[i] = new OtpErlangString(text);
            i++;
        }

        for (final OtpErlangObject arg : args) {
            if (arg instanceof OtpErlangTuple
                    && ((OtpErlangTuple) arg).elementAt(0).equals(
                            new OtpErlangAtom("prompt"))) {
                argsTmp.add(new OtpErlangList(userInput));
                // TODO make it so that it adds input only once, check repeat
                // interactive
            } else {
                argsTmp.add(arg);
            }
        }

        args = argsTmp.toArray(new OtpErlangObject[0]);
    }

    public void addTabWidth(final int tabWidth) {
        final List<OtpErlangObject> argsTmp = Arrays.asList(args);
        argsTmp.add(new OtpErlangInt(tabWidth));

        args = argsTmp.toArray(new OtpErlangObject[0]);
    }

}

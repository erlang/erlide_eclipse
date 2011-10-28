package org.erlide.ui.editors.erl.scanner;

import org.eclipse.jface.text.rules.BufferedRuleBasedScanner;
import org.eclipse.jface.text.rules.Token;
import org.erlide.ui.prefs.TokenHighlight;
import org.erlide.ui.util.IColorManager;

public class ErlStringScanner extends BufferedRuleBasedScanner {

    public ErlStringScanner(final IColorManager colorManager) {
        super();
        final Token defaultToken = ErlCodeScanner
                .getToken(TokenHighlight.STRING.getName());
        setDefaultReturnToken(defaultToken);

        // final Token tildeTag =
        // ErlCodeScanner.getToken(TokenHighlight.EDOC_TAG
        // .getName());
        //
        // final List<IRule> rulesList = Lists.newArrayList();
        // rulesList.add(new SingleLineRule("~", "s", tildeTag));
        //
        // final IRule[] rules = new IRule[rulesList.size()];
        // rulesList.toArray(rules);
        // setRules(rules);
    }

}

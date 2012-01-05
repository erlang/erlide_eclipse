package org.erlide.ui.editors.erl.scanner;

import java.util.List;

import org.eclipse.jface.text.rules.BufferedRuleBasedScanner;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.Token;
import org.erlide.ui.prefs.TokenHighlight;
import org.erlide.ui.util.IColorManager;
import org.erlide.ui.util.text.RegexpRule;

import com.google.common.collect.Lists;

public class ErlStringScanner extends BufferedRuleBasedScanner {

    public ErlStringScanner(final IColorManager colorManager) {
        super();
        final Token defaultToken = ErlCodeScanner
                .getToken(TokenHighlight.STRING.getName());
        setDefaultReturnToken(defaultToken);

        final Token tildeTag = ErlCodeScanner.getToken(TokenHighlight.TILDE_TAG
                .getName());
        final Token escapeTag = ErlCodeScanner
                .getToken(TokenHighlight.ESCAPE_TAG.getName());

        final List<IRule> rulesList = Lists.newArrayList();
        rulesList.add(new RegexpRule(
                "~[0-9*]*(\\.[0-9*]+)?.?t?[~cfegswWpPBbXx#+ni]", tildeTag));
        rulesList.add(new EscapeRule(escapeTag));

        final IRule[] rules = new IRule[rulesList.size()];
        rulesList.toArray(rules);
        setRules(rules);
    }
}

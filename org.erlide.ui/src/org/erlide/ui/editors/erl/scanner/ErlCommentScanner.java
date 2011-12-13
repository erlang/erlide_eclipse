package org.erlide.ui.editors.erl.scanner;

import java.util.List;

import org.eclipse.jface.text.rules.BufferedRuleBasedScanner;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IWordDetector;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WordRule;
import org.erlide.ui.prefs.TokenHighlight;
import org.erlide.ui.util.IColorManager;

import com.google.common.collect.Lists;

public class ErlCommentScanner extends BufferedRuleBasedScanner {

    public ErlCommentScanner(final IColorManager colorManager) {
        super();
        final Token defaultToken = ErlCodeScanner
                .getToken(TokenHighlight.COMMENT.getName());
        setDefaultReturnToken(defaultToken);

        final Token edocTag = ErlCodeScanner.getToken(TokenHighlight.EDOC_TAG
                .getName());
        final Token htmlTag = ErlCodeScanner.getToken(TokenHighlight.HTML_TAG
                .getName());

        final List<IRule> rulesList = Lists.newArrayList();
        rulesList.add(new WordRule(new EdocTagDetector(), edocTag));
        rulesList.add(new SingleLineRule("<", ">", htmlTag));
        final WordRule taskRule = new WordRule(new IWordDetector() {

            @Override
            public boolean isWordStart(final char c) {
                return Character.isLetter(c);
            }

            @Override
            public boolean isWordPart(final char c) {
                return Character.isLetter(c);
            }
        }, defaultToken);
        taskRule.addWord("TODO", edocTag);
        taskRule.addWord("FIXME", edocTag);
        taskRule.addWord("XXX", edocTag);
        rulesList.add(taskRule);

        final IRule[] rules = new IRule[rulesList.size()];
        rulesList.toArray(rules);
        setRules(rules);
    }

    private final class EdocTagDetector implements IWordDetector {
        @Override
        public boolean isWordStart(final char c) {
            return c == '@';
        }

        @Override
        public boolean isWordPart(final char c) {
            return Character.isLowerCase(c);
        }
    }

}

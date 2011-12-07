package org.erlide.ui.editors.erl.scanner;

import java.util.List;

import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.MultiLineRule;
import org.eclipse.jface.text.rules.RuleBasedPartitionScanner;
import org.eclipse.jface.text.rules.Token;

import com.google.common.collect.Lists;

public class ErlangPartitionScanner extends RuleBasedPartitionScanner {

    public ErlangPartitionScanner() {
        super();
        final IToken character = new Token(IErlangPartitions.ERLANG_CHARACTER);
        final IToken string = new Token(IErlangPartitions.ERLANG_STRING);
        final IToken comment = new Token(IErlangPartitions.ERLANG_COMMENT);
        final IToken qatom = new Token(IErlangPartitions.ERLANG_QATOM);

        final List<IRule> rules = Lists.newArrayList();
        rules.add(new ErlangCharRule(character));
        rules.add(new EndOfLineRule("%", comment));
        rules.add(new MultiLineRule("\"", "\"", string, '\\', true));
        rules.add(new MultiLineRule("'", "'", qatom, '\\', true));

        final IPredicateRule[] result = new IPredicateRule[rules.size()];
        rules.toArray(result);
        setPredicateRules(result);
    }

}

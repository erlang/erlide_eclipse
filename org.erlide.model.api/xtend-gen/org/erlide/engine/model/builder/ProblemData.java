package org.erlide.engine.model.builder;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.engine.model.builder.ErlProblems;
import org.erlide.engine.model.builder.ProblemData0;

@SuppressWarnings("all")
public class ProblemData extends ProblemData0 {
  public final static String TAG = "erlide.tag";
  
  public final static String ARGS = "erlide.args";
  
  private Pattern pattern;
  
  public ProblemData(final String tag, final String message, final int arity) {
    super(tag, message, arity);
  }
  
  public Pattern getPattern() {
    boolean _tripleEquals = (this.pattern == null);
    if (_tripleEquals) {
      String _message = this.getMessage();
      final String str = ErlProblems.quoteRegex(_message);
      String _replaceAll = str.replaceAll("~", "(.+?)");
      Pattern _compile = Pattern.compile(_replaceAll);
      this.pattern = _compile;
    }
    return this.pattern;
  }
  
  public void setPattern(final Pattern p) {
    throw new UnsupportedOperationException("pattern is read-only");
  }
  
  public String getCategory() {
    String _tag = this.getTag();
    String[] _split = _tag.split("_");
    return IterableExtensions.<String>head(((Iterable<String>)Conversions.doWrapArray(_split)));
  }
  
  public List<String> getMessageArgs(final String msg) {
    final Matcher matcher = this.pattern.matcher(msg);
    matcher.matches();
    final int num = matcher.groupCount();
    final ArrayList<String> result = CollectionLiterals.<String>newArrayList();
    int i = 1;
    boolean _while = (i <= num);
    while (_while) {
      {
        String _group = matcher.group(i);
        result.add(_group);
        i = (i + 1);
      }
      _while = (i <= num);
    }
    return result;
  }
}

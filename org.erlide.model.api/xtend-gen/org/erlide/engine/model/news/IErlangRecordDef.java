package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangAttribute;
import org.erlide.engine.model.news.IErlangExpression;

@SuppressWarnings("all")
public interface IErlangRecordDef extends IErlangAttribute {
  @NonNull
  public abstract String getName();
  
  @NonNull
  public abstract IErlangExpression getDefinition();
}

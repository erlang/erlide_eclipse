package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangAttribute;
import org.erlide.engine.model.news.IErlangExpression;
import org.erlide.engine.model.news.IErlangFunctionRef;

@SuppressWarnings("all")
public interface IErlangTypeSpec extends IErlangAttribute {
  @NonNull
  public abstract IErlangFunctionRef getFunction();
  
  @NonNull
  public abstract IErlangExpression getSpec();
}

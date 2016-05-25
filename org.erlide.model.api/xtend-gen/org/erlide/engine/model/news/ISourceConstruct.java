package org.erlide.engine.model.news;

import org.erlide.engine.model.news.TextRange;

@SuppressWarnings("all")
public interface ISourceConstruct {
  public abstract TextRange getFullRange();
  
  public abstract TextRange getIdentifyingRange();
}

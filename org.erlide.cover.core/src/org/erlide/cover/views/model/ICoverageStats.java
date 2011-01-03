package org.erlide.cover.views.model;

public interface ICoverageStats {

    public String getLabel();
    public void setLabel(String label);
    
    public int getLinesCount();
    public void setLiniesCount(int count);
    
    public int getCoverCount();
    public void setCoverCount(int count);
    
    public double getPrecentage();
    public void setPercentage(double count);
    
    public String[] getStringArray();
    
}

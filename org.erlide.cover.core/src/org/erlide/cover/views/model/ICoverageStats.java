package org.erlide.cover.views.model;

/**
 * Interface for providing coverage statistics of elements
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 *
 */
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

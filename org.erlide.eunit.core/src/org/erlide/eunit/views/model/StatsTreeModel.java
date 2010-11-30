package org.erlide.eunit.views.model;



public class StatsTreeModel {
	
	private static StatsTreeModel model;
	
	private IStatsTreeObject root;
	
	private StatsTreeModel() {
		initialize();
	}
	
	public static StatsTreeModel getInstance() {
		if(model == null) {
			model = new StatsTreeModel();
		}
		return model;
	}
	
	public IStatsTreeObject getRoot() {
		return root;
	}
	
	private void initialize() {
		System.out.println("Initialize Stats model");
		root = new StatsTreeObject("total", 120, 20, 16);
		
		for(int i = 1; i < 5; i++) {
			IStatsTreeObject module = new StatsTreeObject("module" +i, 12+i, 10, 52);
			root.addChild(module);
			for(int j = 1; j < 3; j++) {
				IStatsTreeObject function = new StatsTreeObject("function"+j, 4+j, 2, 10*j);
				module.addChild(function);
			}
		}
		
	}
	

}

class TT {
	public void m() {
		Test t = new Test();
		System.out.println(t.hello());
	}
  
  public static void main(String... args) {
    new TT().m();
  }
}

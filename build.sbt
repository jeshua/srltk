unmanagedSourceDirectories in Compile <++= baseDirectory { base => Seq(base / "src/main") } 

unmanagedSourceDirectories in Test <++= baseDirectory { base => Seq(base / "src/test/") } 

parallelExecution in Test := false
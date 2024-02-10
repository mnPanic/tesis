theorem "modus ponens":
    (a && (a => b)) => b
proof
    assume "a y a => b": a && a => b;
    thus b by "a y a => b";
end
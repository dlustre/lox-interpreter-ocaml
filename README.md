[![progress-banner](https://backend.codecrafters.io/progress/interpreter/aa212066-c566-4dce-90ca-13f4af5ffce0)](https://app.codecrafters.io/users/codecrafters-bot?r=2qF)

## Design notes

- I had initially built the parser and interpreter as classes -- I didn't have any issues building them this way. However, I was curious to see how they would fare if they were built as first-class modules. Given that I had only implemented block scoping, rebuilding them into first-class modules also turned out fine. I think functors and modules as variables is very interesting, almost feeling like a marriage between object-oriented and metaprogramming. But when I tried to implement functions in a similar fashion to Bob Nystrom's book, it was just too cumbersome to try to make modules work. Here are the issues that I couldn't find an answer to:
  1. Environment values (variables, functions) needed to be generic
  2. Expressions need to also evaluate to functions. 
     
    Since Nystrom's implementation is in Java, he simply type-casts the evaluated expression. But since I represented the 'evaluate' result as variants, I tried to hammer in a 'Function' variant and then I got stuck. The reason this didn't work is because the function's 'call()' needs an interpreter object (an entity that is transitively dependent on the evalulate result type through the environment module) as a parameter. I just couldn't find a way to express that the environment, the interpreter, and the evaluate result type are all mutually recursive (If OCaml had a way to define a module type that was mutually recursive with a type, I think it would have worked.).
- To have something similar to Nystrom's implementation, I have to keep a lot of the types in one file due to them cyclically depending on one another. For instance, I can't keep `stmt` in its own file because the types are dependent as shown: 

  `expr` -> `stmt` -> `interpreter` -> `expr_literal` -> `expr`

- In the beginning, I solely opted for variant types as opposed to option fields since it seemed more idiomatic (nominal over structural) and easier to work with. But in retrospect, I think option fields are easier to reason about (when applicable).

## Other notes

- OCaml error messages can sometimes be undecipherable. Something minor like forgetting parens can just flood the IDE with red lines and display an error message mentioning something that doesn't even seem to relate to your code. Given that this is my first time with OCaml, it was cumbersome trying to debug stuff like this. I wonder if such error handling can be improved on, but my guess is that stuff like this just inevitably happens due to OCaml's type inference constantly trying to figure out what it is that you want to do.
- Personally, it was a so-so experience trying to learn more obscure language features from online resources. Typically, when I want to know how to implement something more nuanced, I'm used to visiting StackOverflow or asking ChatGPT. This of course is a method that works fairly well for more popular languages like JavaScript and C++ -- but for OCaml, it feels just as shabby as trying to learn newly developed languages like Gleam or Odin. However, I'm obliged to say that I eventually found the manual to be pretty helpful sometime later. I think the main reason why I didn't reach for it at first was because it defines its syntax in BNF notation as opposed to code examples which I'm a lot more used to.
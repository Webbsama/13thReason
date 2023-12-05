# Four GoF Patterns

<!-- Sam wants to add a picture of a bridge here for aesthetic purposes. -->
![A photo of a bridge](https://images.unsplash.com/photo-1429041966141-44d228a42775?q=80&w=1000&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxzZWFyY2h8M3x8YnJpZGdlfGVufDB8fDB8fHww)

<!-- This is the first GoF Patterns -->
## 1. Bridge Pattern:
Separates a function's abstraction from its implementation so that the two can vary independently.

### Problem
The Bridge Pattern is utilized to solve problems where there is a need to decouple an abstraction from its implementation. 
It's beneficial when both the abstraction and its implementation need to be extended independently without having their classes closely bound together.

### General Example:
Consider a scenario where you have different types of shapes (abstraction) and various drawing methods (implementation). 
You want the shapes to be drawn in different ways, but you also want to add new shapes or new drawing methods without affecting existing code.

### ERLANG Example
The Bridge pattern in Erlang can be implemented using a combination of modules and functions to separate abstraction from implementation.
This pattern is useful when you want to decouple an abstraction from its implementation, allowing them to vary independently. 
In Erlang, this can be achieved using modules to represent abstraction and implementation, and functions to define the interface between them.


<!-- Sam wants to add a picture of a chain here for aesthetic purposes. -->
![A photo of a chain](https://static8.depositphotos.com/1377527/927/i/450/depositphotos_9278035-stock-photo-metal-chain.jpg)

<!-- This is the second GoF Patterns -->
## 2. Chain of Responsibility Pattern:
Passes requests along a chain of functions. Upon receiving a request, each function decides either to process the request or to pass it along the chain.

### Problem
The Chain of Responsibility Pattern is used when there is a chain of handlers for a request and the handling logic can be different for each handler. 
It's helpful in scenarios where a request needs to be processed by one of multiple handlers without explicitly specifying the receiver.

### General Example:
Consider an application where a user input needs to go through several processing stages (e.g., validation, authentication, authorization). 
Each stage represents a handler in a chain. 
The input passes through these handlers, and each handler decides whether it can handle the request or if it needs to pass it to the next handler in the chain.

### ERLANG Example
The Chain of Responsibility pattern in Erlang can be implemented using a combination of functions and message passing. 
In Erlang, processes and message passing are often used for creating concurrent and distributed systems. 
The basic idea is to create a chain of processes, each capable of handling a specific type of request. 
If a process cannot handle the request, it forwards the request to the next process in the chain.

<!-- Sam wants to add a picture of a builder here for aesthetic purposes. -->
![A photo of a Bob the Builder](https://upload.wikimedia.org/wikipedia/en/thumb/c/c5/Bob_the_builder.jpg/220px-Bob_the_builder.jpg)

<!-- This is the third GoF Patterns -->
## 3. Builder Pattern:
Separates the construction of a complex structure from its representation, allowing the same construction process to create different representations.

### Problem
The builder pattern is used when a need to add additional components to a program (such as a class). 
If you use a builder function to create a program you can produce a final product step by step and create different configurations of the same object. 
This allows you to implement optional components without bloating the constructor with a long list of parameters. 
Consider building an order for a meal, you have multiple options to create your final product. 

### ERlANG Example
The builder pattern can be represented in erlang by using a function that utilizes smaller helper functions that create different results depending on the state of the function. 
From that you can receive multiple options, this will create multiple processes, but it will still keep the initial function from being bloated. 
You just need to utilize multiple helper functions to aid in the initial "builder" function.

<!-- Sam wants to add a picture of an interpreter here for aesthetic purposes. -->
![A photo of an interpreter](https://www.blueridge.edu/wp-content/uploads/2020/03/GettyImages-184376841-862x485.jpg)

<!-- This is the fourth GoF Patterns -->
## 4. Interpreter Pattern:
Defines a grammar for the language and a function to interpret sentences in the language.

### Problem
The Builder Pattern is employed when there's a need to construct complex objects step by step. 
It's useful when the construction process should allow different representations of the object without the construction process becoming too complex or coupled to a specific representation.

### General Example
Consider the construction of an HTML document. The document may contain different elements like headers, paragraphs, lists, etc. 
Using the Builder Pattern, you can have a builder interface defining steps to create these elements and concrete builders implementing these steps. 
This way, the same construction process can create different HTML representations.

### ERLANG Example
In Erlang, the Builder pattern is often implemented using a combination of functions and data structures to construct complex objects or structures. 
The key idea behind the Builder pattern is to separate the construction of a complex object from its representation, allowing the same construction process to create different representations.


<!-- Proof of our work this semester. 
Sam also thinks that she is funny. -->
**Written by the 13th Reason Group.**

![A gif of Elmo surrounded by fire](https://media.tenor.com/AVCT2wSkaWUAAAAC/elmo-fire.gif)
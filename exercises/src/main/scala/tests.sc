object tests {
 
  case class Town(name: String, pizzerias: Set[Pizzeria])
  case class Pizzeria(name: String, pizzas: Set[Pizza])
  case class Pizza(name: String, ingredients: Set[Ingredient])
  case class Ingredient(name: String, isVegetarian: Boolean)

  val onion = Ingredient("onion", true)           //> onion  : tests.Ingredient = Ingredient(onion,true)
  val pepperoni = Ingredient("pepperoni", true)   //> pepperoni  : tests.Ingredient = Ingredient(pepperoni,true)
  val mushroom = Ingredient("mushroom", true)     //> mushroom  : tests.Ingredient = Ingredient(mushroom,true)
  val tomato = Ingredient("tomato", true)         //> tomato  : tests.Ingredient = Ingredient(tomato,true)
  val spinach = Ingredient("spinach", true)       //> spinach  : tests.Ingredient = Ingredient(spinach,true)
  val avocado = Ingredient("avocado", true)       //> avocado  : tests.Ingredient = Ingredient(avocado,true)
  val garlic = Ingredient("garlic", false)        //> garlic  : tests.Ingredient = Ingredient(garlic,false)
  val mozarella = Ingredient("mozarella", true)   //> mozarella  : tests.Ingredient = Ingredient(mozarella,true)
  val parmesan = Ingredient("parmesan", true)     //> parmesan  : tests.Ingredient = Ingredient(parmesan,true)
  val ricotta = Ingredient("ricotta", true)       //> ricotta  : tests.Ingredient = Ingredient(ricotta,true)
  val shrimp = Ingredient("ham", true)            //> shrimp  : tests.Ingredient = Ingredient(ham,true)
  val ham = Ingredient("ham", false)              //> ham  : tests.Ingredient = Ingredient(ham,false)
  val chicken = Ingredient("chicken", false)      //> chicken  : tests.Ingredient = Ingredient(chicken,false)

  val veggie = Pizza("veggie", Set(tomato, onion, mozarella, mushroom,pepperoni))
                                                  //> veggie  : tests.Pizza = Pizza(veggie,Set(Ingredient(tomato,true), Ingredient
                                                  //| (pepperoni,true), Ingredient(mozarella,true), Ingredient(mushroom,true), Ing
                                                  //| redient(onion,true)))
  val chickenAvocado = Pizza("chickenAvocado", Set(chicken, avocado, garlic,pepperoni))
                                                  //> chickenAvocado  : tests.Pizza = Pizza(chickenAvocado,Set(Ingredient(chicken,
                                                  //| false), Ingredient(avocado,true), Ingredient(garlic,false), Ingredient(peppe
                                                  //| roni,true)))
  val calzone = Pizza("calzone", Set(ricotta, pepperoni, mushroom))
                                                  //> calzone  : tests.Pizza = Pizza(calzone,Set(Ingredient(ricotta,true), Ingred
                                                  //| ient(pepperoni,true), Ingredient(mushroom,true)))
  val seaFood = Pizza("sea food", Set(parmesan, garlic, spinach, shrimp))
                                                  //> seaFood  : tests.Pizza = Pizza(sea food,Set(Ingredient(parmesan,true), Ingr
                                                  //| edient(garlic,false), Ingredient(spinach,true), Ingredient(ham,true)))
  val hamPiz = Pizza("hamPiz", Set(onion, parmesan, ham, mozarella,pepperoni))
                                                  //> hamPiz  : tests.Pizza = Pizza(hamPiz,Set(Ingredient(pepperoni,true), Ingred
                                                  //| ient(ham,false), Ingredient(mozarella,true), Ingredient(parmesan,true), Ing
                                                  //| redient(onion,true)))
  val margherita = Pizza("margherita", Set(garlic, mozarella, parmesan, tomato))
                                                  //> margherita  : tests.Pizza = Pizza(margherita,Set(Ingredient(garlic,false), 
                                                  //| Ingredient(mozarella,true), Ingredient(parmesan,true), Ingredient(tomato,tr
                                                  //| ue)))

  val lucio = Pizzeria("Lucio", Set(veggie, chickenAvocado, hamPiz, veggie))
                                                  //> lucio  : tests.Pizzeria = Pizzeria(Lucio,Set(Pizza(veggie,Set(Ingredient(to
                                                  //| mato,true), Ingredient(pepperoni,true), Ingredient(mozarella,true), Ingredi
                                                  //| ent(mushroom,true), Ingredient(onion,true))), Pizza(chickenAvocado,Set(Ingr
                                                  //| edient(chicken,false), Ingredient(avocado,true), Ingredient(garlic,false), 
                                                  //| Ingredient(pepperoni,true))), Pizza(hamPiz,Set(Ingredient(pepperoni,true), 
                                                  //| Ingredient(ham,false), Ingredient(mozarella,true), Ingredient(parmesan,true
                                                  //| ), Ingredient(onion,true)))))
  val laScala = Pizzeria("La Scala", Set(calzone, chickenAvocado, hamPiz))
                                                  //> laScala  : tests.Pizzeria = Pizzeria(La Scala,Set(Pizza(calzone,Set(Ingredi
                                                  //| ent(ricotta,true), Ingredient(pepperoni,true), Ingredient(mushroom,true))),
                                                  //|  Pizza(chickenAvocado,Set(Ingredient(chicken,false), Ingredient(avocado,tru
                                                  //| e), Ingredient(garlic,false), Ingredient(pepperoni,true))), Pizza(hamPiz,Se
                                                  //| t(Ingredient(pepperoni,true), Ingredient(ham,false), Ingredient(mozarella,t
                                                  //| rue), Ingredient(parmesan,true), Ingredient(onion,true)))))
  val ragazza = Pizzeria("Ragazza", Set(calzone, chickenAvocado, hamPiz))
                                                  //> ragazza  : tests.Pizzeria = Pizzeria(Ragazza,Set(Pizza(calzone,Set(Ingredie
                                                  //| nt(ricotta,true), Ingredient(pepperoni,true), Ingredient(mushroom,true))), 
                                                  //| Pizza(chickenAvocado,Set(Ingredient(chicken,false), Ingredient(avocado,true
                                                  //| ), Ingredient(garlic,false), Ingredient(pepperoni,true))), Pizza(hamPiz,Set
                                                  //| (Ingredient(pepperoni,true), Ingredient(ham,false), Ingredient(mozarella,tr
                                                  //| ue), Ingredient(parmesan,true), Ingredient(onion,true)))))
  val juliana = Pizzeria("Juliana", Set(calzone, chickenAvocado, hamPiz, margherita))
                                                  //> juliana  : tests.Pizzeria = Pizzeria(Juliana,Set(Pizza(calzone,Set(Ingredie
                                                  //| nt(ricotta,true), Ingredient(pepperoni,true), Ingredient(mushroom,true))), 
                                                  //| Pizza(chickenAvocado,Set(Ingredient(chicken,false), Ingredient(avocado,true
                                                  //| ), Ingredient(garlic,false), Ingredient(pepperoni,true))), Pizza(hamPiz,Set
                                                  //| (Ingredient(pepperoni,true), Ingredient(ham,false), Ingredient(mozarella,tr
                                                  //| ue), Ingredient(parmesan,true), Ingredient(onion,true))), Pizza(margherita,
                                                  //| Set(Ingredient(garlic,false), Ingredient(mozarella,true), Ingredient(parmes
                                                  //| an,true), Ingredient(tomato,true)))))
  val turino = Pizzeria("Turino", Set(calzone, hamPiz, seaFood))
                                                  //> turino  : tests.Pizzeria = Pizzeria(Turino,Set(Pizza(calzone,Set(Ingredient
                                                  //| (ricotta,true), Ingredient(pepperoni,true), Ingredient(mushroom,true))), Pi
                                                  //| zza(hamPiz,Set(Ingredient(pepperoni,true), Ingredient(ham,false), Ingredien
                                                  //| t(mozarella,true), Ingredient(parmesan,true), Ingredient(onion,true))), Piz
                                                  //| za(sea food,Set(Ingredient(parmesan,true), Ingredient(garlic,false), Ingred
                                                  //| ient(spinach,true), Ingredient(ham,true)))))

  val pizzerias = Set(lucio, laScala, ragazza, juliana, turino)
                                                  //> pizzerias  : scala.collection.immutable.Set[tests.Pizzeria] = Set(Pizzeria(
                                                  //| Ragazza,Set(Pizza(calzone,Set(Ingredient(ricotta,true), Ingredient(pepperon
                                                  //| i,true), Ingredient(mushroom,true))), Pizza(chickenAvocado,Set(Ingredient(c
                                                  //| hicken,false), Ingredient(avocado,true), Ingredient(garlic,false), Ingredie
                                                  //| nt(pepperoni,true))), Pizza(hamPiz,Set(Ingredient(pepperoni,true), Ingredie
                                                  //| nt(ham,false), Ingredient(mozarella,true), Ingredient(parmesan,true), Ingre
                                                  //| dient(onion,true))))), Pizzeria(Juliana,Set(Pizza(calzone,Set(Ingredient(ri
                                                  //| cotta,true), Ingredient(pepperoni,true), Ingredient(mushroom,true))), Pizza
                                                  //| (chickenAvocado,Set(Ingredient(chicken,false), Ingredient(avocado,true), In
                                                  //| gredient(garlic,false), Ingredient(pepperoni,true))), Pizza(hamPiz,Set(Ingr
                                                  //| edient(pepperoni,true), Ingredient(ham,false), Ingredient(mozarella,true), 
                                                  //| Ingredient(parmesan,true), Ingredient(onion,true))), Pizza(margherita,Set(I
                                                  //| ngredient(garlic,false)
                                                  //| Output exceeds cutoff limit.

  // map doesn't work here as we get each time a Set. The result are embedded Sets: Set[Set[Set[String]]
  val allIngredientsKo = pizzerias.map(pizzeria =>
  	pizzeria.pizzas.map(pizza =>
  		pizza.ingredients.map(ingredient => ingredient.name)
  	)
  )                                               //> allIngredientsKo  : scala.collection.immutable.Set[scala.collection.immutab
                                                  //| le.Set[scala.collection.immutable.Set[String]]] = Set(Set(Set(ricotta, pepp
                                                  //| eroni, mushroom), Set(chicken, avocado, garlic, pepperoni), Set(pepperoni, 
                                                  //| mozarella, ham, parmesan, onion)), Set(Set(ricotta, pepperoni, mushroom), S
                                                  //| et(chicken, avocado, garlic, pepperoni), Set(pepperoni, mozarella, ham, par
                                                  //| mesan, onion), Set(garlic, mozarella, parmesan, tomato)), Set(Set(ricotta, 
                                                  //| pepperoni, mushroom), Set(pepperoni, mozarella, ham, parmesan, onion), Set(
                                                  //| parmesan, garlic, spinach, ham)), Set(Set(pepperoni, mozarella, tomato, mus
                                                  //| hroom, onion), Set(chicken, avocado, garlic, pepperoni), Set(pepperoni, moz
                                                  //| arella, ham, parmesan, onion)))
  // We need to flatten the Sets each time:
  val allIngredients = pizzerias.flatMap(pizzeria =>
  	pizzeria.pizzas.flatMap(pizza =>
  		pizza.ingredients.map(ingredient => ingredient.name)
  	)
  )                                               //> allIngredients  : scala.collection.immutable.Set[String] = Set(pepperoni, g
                                                  //| arlic, mozarella, ham, tomato, spinach, avocado, parmesan, chicken, mushroo
                                                  //| m, onion, ricotta)

  // With a for comprehension:
  val allIngredientsWithFor = for {
    pizzeria <- pizzerias
    pizza <- pizzeria.pizzas
    ingredient <- pizza.ingredients
  } yield (ingredient.name)                       //> allIngredientsWithFor  : scala.collection.immutable.Set[String] = Set(peppe
                                                  //| roni, garlic, mozarella, ham, tomato, spinach, avocado, parmesan, chicken, 
                                                  //| mushroom, onion, ricotta)

// Get all the names of the pizzerias that propose pizzas with pepperoni along with the pizza name and its ingredients:
  val PizzeriasveggyPizzas = pizzerias.flatMap(pizzeria =>
  	pizzeria.pizzas.filter(p => p.ingredients.exists(_.name == "pepperoni")).map(pizza =>
  		(pizzeria.name, pizza.name)
  	)
  )                                               //> PizzeriasveggyPizzas  : scala.collection.immutable.Set[(String, String)] = 
                                                  //| Set((La Scala,calzone), (Turino,calzone), (Juliana,hamPiz), (Ragazza,calzon
                                                  //| e), (Turino,hamPiz), (Lucio,hamPiz), (Ragazza,chickenAvocado), (Juliana,chi
                                                  //| ckenAvocado), (Ragazza,hamPiz), (Lucio,veggie), (La Scala,chickenAvocado), 
                                                  //| (Juliana,calzone), (La Scala,hamPiz), (Lucio,chickenAvocado))
  // With a for comprehension:
  val PizzeriasveggyPizzasWithFor = for {
    pizzeria <- pizzerias
    pizza <- pizzeria.pizzas if pizza.ingredients.exists(_.name == "pepperoni")
    ingredient <- pizza.ingredients
  } yield (pizzeria.name, pizza.name)             //> PizzeriasveggyPizzasWithFor  : scala.collection.immutable.Set[(String, Stri
                                                  //| ng)] = Set((La Scala,calzone), (Turino,calzone), (Juliana,hamPiz), (Ragazza
                                                  //| ,calzone), (Turino,hamPiz), (Lucio,hamPiz), (Ragazza,chickenAvocado), (Juli
                                                  //| ana,chickenAvocado), (Ragazza,hamPiz), (Lucio,veggie), (La Scala,chickenAvo
                                                  //| cado), (Juliana,calzone), (La Scala,hamPiz), (Lucio,chickenAvocado))

}
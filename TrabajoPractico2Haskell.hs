    
    sumatoria :: [Int] -> Int 
    sumatoria [] = 0
    sumatoria (x:xs) = x + sumatoria xs
    
    longitud :: [a] -> Int
    longitud [] = 0
    longitud (x:xs) = 1 + longitud xs
    
    sucesor :: [Int] -> [Int]
    sucesor [] = []
    sucesor (x:xs) = (x+1) : sucesor xs
    
    conjuncion :: [Bool] -> Bool
    conjuncion [] = True 
    conjuncion (b:bs) = b && conjuncion bs
    
    disyuncion :: [Bool] -> Bool
    disyuncion [] = False 
    disyuncion (b:bs) = b || disyuncion bs
    
    aplanar :: [[a]] -> [a]
    aplanar []=[]
    aplanar (xs:xss) = xs ++ aplanar xss
    
    pertenece :: Eq a => a -> [a] -> Bool
    pertenece e [] = False
    pertenece e (x:xs) = e==x || pertenece e xs
    
    apariciones :: Eq a => a -> [a] -> Int 
    apariciones e [] = 0
    apariciones e (x:xs) = unoSiEsIgual e x + apariciones e xs
    
    unoSiEsIgual :: Eq a => a -> a -> Int
    unoSiEsIgual e1 e2 = if sonIguales e1 e2 then 1 else 0 
    
    sonIguales :: Eq a => a -> a -> Bool
    sonIguales e1 e2 = e1 == e2 

    losMenoresA :: Int -> [Int] -> [Int]
    losMenoresA n [] = []
    losMenoresA n (x:xs) = if x < n then x : losMenoresA n xs 
                                    else losMenoresA n xs
    
    lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
    lasDeLongitudMayorA n [] = []
    lasDeLongitudMayorA n (x:xs) = if longitud x > n 
                                                then [x] ++ lasDeLongitudMayorA n xs
                                                else lasDeLongitudMayorA n xs
    
    agregarAlFinal :: [a] -> a -> [a]
    agregarAlFinal [] e = [e]
    agregarAlFinal  (x:xs) e = x:(agregarAlFinal xs e)
    
    
    agregar :: [a] -> [a] -> [a]
    agregar [] ys = ys
    agregar (x:xs) ys = x : agregar xs ys
    
    reversa :: [a] -> [a]
    reversa [] = []
    reversa (x:xs) = reversa xs++[x]
    
    zipMaximos :: [Int] -> [Int] -> [Int]
    zipMaximos [] ys = ys 
    zipMaximos xs [] = xs
    zipMaximos (x:xs) (y:ys) = if x > y then x : zipMaximos xs ys 
                                        else y : zipMaximos xs ys
    
    elMinimo :: Ord a => [a] -> a
    elMinimo [] = error "la lista esta vacia"
    elMinimo (x:xs) = if null xs || x < elMinimo xs then x else elMinimo xs
    
    factorial :: Int -> Int
    factorial n = if n > 0 then n * (factorial (n-1)) else 1 
    
    cuentaRegresiva :: Int -> [Int]
    cuentaRegresiva n = if n < 1 then [] else n:cuentaRegresiva (n-1)
    
    repetir  :: Int -> a -> [a] 
    repetir 0 a = []
    repetir n a = a:(repetir (n-1) a)
    
    losPrimeros :: Int -> [a] -> [a]
    losPrimeros n [] = []
    losPrimeros n (x:xs) = if n == 0 then  []
                               else x : losPrimeros (n-1) xs   
    
    
    sinLosPrimeros :: Int -> [a] -> [a]
    sinLosPrimeros 0 xs = xs
    sinLosPrimeros n (x:xs) = if n>=1 then sinLosPrimeros (n-1) xs else xs 
    
    
    
    
    data Persona = P Nombre Edad deriving Show
    type Nombre = String
    type Edad = Int 
    
    nombre :: Persona -> String
    nombre (P n e) = n
    
    edad :: Persona -> Int 
    edad (P n e) = e

    mayoresA :: Int -> [Persona] -> [Persona]
    mayoresA n [] = []
    mayoresA n (x:xs) = if edad x > n then x : mayoresA n xs 
                                      else mayoresA n xs
    
    promedioEdad :: [Persona] -> Int
    promedioEdad [] = error "no hay ninguna persona"
    promedioEdad xs = div(edades xs) (longitud xs)
    
    edades :: [Persona] -> Int
    edades [] = 0
    edades (x:xs) = (edad x) + edades xs 
    
    elMasViejo :: [Persona] -> Persona
    elMasViejo [] = error "no hay ninguna persona"
    elMasViejo [x] = x
    elMasViejo (x:xs) = if edad x > edad (elMasViejo xs) then x else elMasViejo xs
    
    
    
    data TipoDePokemon = Agua | Fuego | Planta deriving (Eq,Show)
    data Pokemon = ConsPokemon TipoDePokemon Int  deriving Show
    data Entrenador = ConsEntrenador String [Pokemon]  deriving Show
    
    cantPokemon :: Entrenador -> Int
    cantPokemon e = length (pokemonDeEntrenador e)
    
    --proposito : dado un Entrenador retorna una lista con sus Pokemon
    pokemonDeEntrenador :: Entrenador -> [Pokemon]
    pokemonDeEntrenador (ConsEntrenador n ps) = ps
    
    cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
    cantPokemonDe t (ConsEntrenador n ps) = length (todosLosPokemonDeTipo t ps)
    
    todosLosPokemonDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
    todosLosPokemonDeTipo t [] = []
    todosLosPokemonDeTipo t (x:xs) = if t == (tipoDePokemon x) 
                                                    then x : (todosLosPokemonDeTipo t xs)
                                                    else todosLosPokemonDeTipo t xs
    
    tipoDePokemon :: Pokemon -> TipoDePokemon
    tipoDePokemon (ConsPokemon t e) = t
    

    losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
    losQueLeGanan t e1 e2 = length (losQueLeGananAAlguno(pokemonDeTipo t (pokemonDeEntrenador e1)) (pokemonDeEntrenador e2))
                            
    
    
    --proposito : dado dos Pokemon indica si el primero le gana al segundo
    superaA :: Pokemon -> Pokemon -> Bool
    superaA p1 p2 = tipoSuperior (tipoDePokemon p1) (tipoDePokemon p2)
    
    tipoSuperior :: TipoDePokemon ->TipoDePokemon -> Bool
    tipoSuperior Agua Fuego = True 
    tipoSuperior Fuego Planta = True 
    tipoSuperior Planta Agua = True 
    tipoSuperior _ _ = False 
    
    pokemonDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
    pokemonDeTipo t [] = []
    pokemonDeTipo t (x:xs) =  if (tipoDePokemon x) == t then x : pokemonDeTipo t xs
                                                        else  pokemonDeTipo t xs
    
    leGanaAAlguno :: Pokemon -> [Pokemon]->Bool
    leGanaAAlguno p [] = False
    leGanaAAlguno p (x:xs) = superaA  p x || leGanaAAlguno p xs
    
    losQueLeGananAAlguno :: [Pokemon] -> [Pokemon] -> [Pokemon]
    losQueLeGananAAlguno [] ys = []
    losQueLeGananAAlguno (x:xs) ys = if leGanaAAlguno x ys 
                                                    then x : losQueLeGananAAlguno xs ys        
                                                    else losQueLeGananAAlguno xs ys    
    
    cantidadDeTipo :: TipoDePokemon -> [Pokemon] -> Int
    cantidadDeTipo t [] = 0
    cantidadDeTipo t (x:xs) = if (tipoDePokemon x) == t then 1 + cantidadDeTipo t xs
                                             else  cantidadDeTipo t xs
    
    esMaestroPokemon :: Entrenador -> Bool
    esMaestroPokemon p = poseeDeTodosLosTipos (pokemonDeEntrenador p)
    
    poseeUnoDeTipo:: TipoDePokemon -> [Pokemon]-> Bool
    poseeUnoDeTipo t [] = False
    poseeUnoDeTipo t (x:xs) = t ==(tipoDePokemon x)||poseeUnoDeTipo t xs 
    
    poseeDeTodosLosTipos :: [Pokemon]->Bool
    poseeDeTodosLosTipos xs = poseeUnoDeTipo Agua xs && 
                              poseeUnoDeTipo Fuego xs && 
                              poseeUnoDeTipo Planta xs
    
---------------------------------------------------------------------------------------------------------
    
    data Seniority = Junior | SemiSenior | Senior deriving Show
    data Proyecto = ConsProyecto String  deriving (Show)
    data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
    data Empresa = ConsEmpresa [Rol] deriving Show
    
    
    proyectos :: Empresa -> [Proyecto]
    proyectos (ConsEmpresa xs) = sinProyectosRepetidos(proyectosDeRoles xs)
    
    
    proyectosDeRoles :: [Rol] -> [Proyecto]
    proyectosDeRoles [] = []
    proyectosDeRoles (x:xs) = (proyectoDeRol x): proyectosDeRoles xs
    
    proyectoDeRol :: Rol -> Proyecto
    proyectoDeRol (Developer s p) = p
    proyectoDeRol (Management s p) = p
    
    sinProyectosRepetidos :: [Proyecto] -> [Proyecto]
    sinProyectosRepetidos [] = []
    sinProyectosRepetidos (p:ps) =  quitarProyectoSiPertenece p (sinProyectosRepetidos ps)
    
    quitarProyectoSiPertenece :: Proyecto -> [Proyecto] -> [Proyecto]
    quitarProyectoSiPertenece pr [] = []
    quitarProyectoSiPertenece pr (p:ps) = if esMismoProyecto pr p then ps 
                                                                                                else p : quitarProyectoSiPertenece pr ps 
    


    losDevSenior :: Empresa -> [Proyecto] -> Int
    losDevSenior (ConsEmpresa ps) xs = losQuePertenecenAlProyecto (desarrolladoresSenior ps) xs  
    
    losQuePertenecenAlProyecto :: [Rol] -> [Proyecto] -> Int 
    losQuePertenecenAlProyecto [] ps = 0
    losQuePertenecenAlProyecto (r:rs) ps = (unoSiperteneceAlProyecto r ps) + (losQuePertenecenAlProyecto rs ps) 
    
    unoSiperteneceAlProyecto :: Rol -> [Proyecto] -> Int
    unoSiperteneceAlProyecto r ps = if perteneceAlProyecto r ps then 1 else 0

  
    --proposito : dado un Rol devuelve una lista con ese Rol si el desarrollador es Senior
    desarrolladoresSenior :: [Rol] -> [Rol]
    desarrolladoresSenior [] = []
    desarrolladoresSenior (x:xs) = (desarrolladorSenior x) ++ (desarrolladoresSenior xs)
    
    desarrolladorSenior :: Rol -> [Rol]
    desarrolladorSenior r = if esDev r && esRolSenior r then [r] 
                                             else []
                                             
    esDev :: Rol -> Bool
    esDev (Developer s p) = True
    esDev _ = False
    
    esRolSenior :: Rol -> Bool
    esRolSenior (Developer s p)  = esSenior s
    esRolSenior (Management s p) = esSenior s 
    
    esSenior :: Seniority -> Bool
    esSenior Senior = True
    esSenior _ = False
    
    perteneceAlProyecto :: Rol -> [Proyecto] -> Bool
    perteneceAlProyecto r [] = False
    perteneceAlProyecto r (p:ps) =  esProyectoDeRol r p ||  perteneceAlProyecto r ps
    
    esProyectoDeRol :: Rol -> Proyecto -> Bool
    esProyectoDeRol r p = esMismoProyecto (proyectoDeRol r) p

    cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
    cantQueTrabajanEn ps (ConsEmpresa rs) = cantRolQueParticipan rs ps


    losDeRolDesarrollador :: [Rol] -> [Rol]
    losDeRolDesarrollador [] = []
    losDeRolDesarrollador (r:rs) = desarrollador r ++ losDeRolDesarrollador rs 
    
    desarrollador :: Rol -> [Rol]
    desarrollador r = if esDev r then [r] 
                                    else [] 

    cantRolQueParticipan :: [Rol] -> [Proyecto] -> Int
    cantRolQueParticipan [] ps = 0
    cantRolQueParticipan (r:rs) ps = if perteneceAlProyecto r ps 
                                then 1 + cantRolQueParticipan rs ps
                                else cantRolQueParticipan rs ps
    
    ----------------------------------------------------------------------------------------
    
    asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
    asignadosPorProyecto (ConsEmpresa rs) = procesarProyectos rs 

    
    
    procesarProyectos :: [Rol]->[(Proyecto,Int)]
    procesarProyectos [] = []
    procesarProyectos (r:rs) = incluirElProyectoDe r (procesarProyectos rs) 

    incluirElProyectoDe:: Rol -> [(Proyecto,Int)] -> [(Proyecto,Int)] 
    incluirElProyectoDe r [] = [(proyectoDeRol r,1)]
    incluirElProyectoDe r ((p,n):pns) =  if esMismoProyecto (proyectoDeRol r ) p  
                                                                then (p,n+1) : incluirElProyectoDe r pns
                                                                else  (p,n) : incluirElProyectoDe r pns
 
    esMismoProyecto :: Proyecto -> Proyecto -> Bool
    esMismoProyecto p1 p2 = nombreDeProyecto p1 == nombreDeProyecto p2
    
    nombreDeProyecto :: Proyecto -> String
    nombreDeProyecto (ConsProyecto s) = s

    pr1 = ConsProyecto "cajero"
    pr2 = ConsProyecto "banco"
    pr3 = ConsProyecto "municipalidad"
    pr4 =  ConsProyecto "universidad"
    
    desa1= Developer Junior pr1 
    desa2= Developer SemiSenior pr1
    desa3= Developer Senior pr2
    desa4= Developer Senior pr4
    mana = Management Senior pr1
    desa5 = Developer SemiSenior pr4
    
    empresa = ConsEmpresa [desa1,desa2,desa3,desa4,mana,desa5]
    empresa2 = ConsEmpresa []
    variosProyectos = [pr4]
    
    kyogre = ConsPokemon Agua 70
    moltres = ConsPokemon Fuego 50
    charmander = ConsPokemon Fuego 10 
    bulbasaur = ConsPokemon Planta 12
    squirtle = ConsPokemon Agua 7 
    
    ash = ConsEntrenador "Ash" [charmander,moltres]
    gary = ConsEntrenador "Gary" [squirtle,squirtle]
    maximo = ConsEntrenador "Maximo" [kyogre,moltres,bulbasaur]
    chano = ConsEntrenador "Chano" []
    
    jose = P "Jose" 18
    juan = P "Juan" 28
    miguel = P "Miguel" 38
    
    multitud = [jose,juan,miguel]


let card_dir_1 = ""; 
let card_dir_2 = ""; 
let vector_dir =["Alcaldía"]; 

 function mostrar_dir_1 (){
   card_dir_1= 'Alcaldía';
   vector_dir[0] = card_dir_1;
   Shiny.setInputValue("dir_vector", vector_dir);
  }
        
 function mostrar_dir_2 (){
   card_dir_2= 'Sectores';
   vector_dir[0] = card_dir_2;
   Shiny.setInputValue("dir_vector", vector_dir);
  }
 
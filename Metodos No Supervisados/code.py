"""
Autor: Jose Daniel Requena
Fecha: Abril 2025

Descripción:
Algoritmo Apriori para encontrar itemsets frecuentes, cerrados y máximos
Este código implementa el algoritmo Apriori para encontrar itemsets frecuentes, cerrados y máximos a partir de un conjunto de transacciones.
El algoritmo se basa en la idea de que un subconjunto de un itemset frecuente también debe ser frecuente.
El código está dividido en varias funciones para facilitar su comprensión y reutilización.
Se utiliza la biblioteca itertools para generar combinaciones de elementos y la biblioteca collections para contar frecuencias de manera eficiente.
"""
# Importamos las bibliotecas necesarias 
import csv
from itertools import combinations
from collections import defaultdict # Para contar frecuencias de manera eficiente

# Paso 1: Cargar las transacciones desde un archivo CSV
def load_transactions(filepath):
    """
    Carga las transacciones desde un archivo CSV.

    Args:
        filepath (str): Ruta al archivo CSV.

    Returns:
        list: Lista de transacciones, donde cada transacción es una lista de elementos.
    """
    transactions = [] # Lista para almacenar las transacciones
    with open(filepath, 'r') as file:
        reader = csv.reader(file)
        for row in reader:
            # Limpiamos los elementos de la fila eliminando espacios y descartando valores vacíos
            transaction = [item.strip() for item in row if item.strip()]
            transactions.append(transaction) # Agregamos la transacción a la lista
    return transactions # Devolvemos la lista de transacciones

# Paso 2: Generar todos los itemsets frecuentes hasta cierto soporte mínimo
def generate_itemsets(transactions, min_support):
    """
    Genera los itemsets frecuentes a partir de las transacciones.

    Args:
        transactions (list): Lista de transacciones.
        min_support (int): Soporte mínimo para considerar un itemset como frecuente.

    Returns:
        dict: Diccionario con los itemsets frecuentes y sus frecuencias.
    """
    itemsets = defaultdict(int) # Diccionario para contar la frecuencia de cada itemset
    
    # Contamos la frecuencia de los itemsets
    for transaction in transactions:
        for length in range(1, len(transaction) + 1):
            for itemset in combinations(sorted(transaction), length): # Generamos combinaciones ordenadas
                itemsets[itemset] += 1
                
    # Filtramos los itemsets por el umbral de soporte mínimo
    frequent_itemsets = {itemset: count for itemset, count in itemsets.items() if count >= min_support}
    
    return frequent_itemsets # Devolvemos los itemsets frecuentes

# Paso 3: Funciones para obtener itemsets cerrados y máximos
def get_closed_itemsets(frequent_itemsets):
    """
    Obtiene los itemsets cerrados a partir de los itemsets frecuentes.

    Args:
        frequent_itemsets (dict): Diccionario de itemsets frecuentes con sus frecuencias.

    Returns:
        list: Lista de itemsets cerrados.
    """
    closed_itemsets = set(frequent_itemsets.keys()) # Inicialmente, todos los itemsets son cerrados
    
    for itemset in list(frequent_itemsets):
        for subset in combinations(itemset, len(itemset) - 1): # Generamos todos los subconjuntos
            # Si un subconjunto tiene la misma frecuencia que el itemset, no es cerrado
            if subset in frequent_itemsets and frequent_itemsets[subset] == frequent_itemsets[itemset]:
                closed_itemsets.discard(itemset)  # Si el conjunto tiene un superconjunto con la misma frecuencia, lo eliminamos
    
    return list(closed_itemsets) # Devolvemos la lista de itemsets cerrados

def get_maximal_itemsets(frequent_itemsets):
    """
    Obtiene los itemsets máximos a partir de los itemsets frecuentes.

    Args:
        frequent_itemsets (dict): Diccionario de itemsets frecuentes con sus frecuencias.

    Returns:
        list: Lista de itemsets máximos.
    """
    maximal_itemsets = set(frequent_itemsets.keys()) # Inicialmente, todos los itemsets son máximos
    
    for itemset in list(frequent_itemsets):
        for larger_itemset in list(frequent_itemsets):
            # Si un itemset es subconjunto de otro, no es máximo
            if itemset != larger_itemset and set(itemset).issubset(set(larger_itemset)):
                maximal_itemsets.discard(itemset)  # Si un conjunto es un subconjunto de otro, lo eliminamos
    
    return list(maximal_itemsets) # Devolvemos la lista de itemsets máximos

# Paso 4: Función principal para ejecutar el algoritmo
if __name__ == "__main__":
    """
    Ejemplo de uso:
    Asegurarse de que el archivo transactions.csv esté en el mismo directorio
    o se tendra que proporcionar la ruta correcta al archivo.
    transactions.csv debe contener las transacciones en formato CSV
    
    Ejemplo de archivo CSV:
    item1,item2,item3
    """
    
    # Paso 1: Cargar las transacciones desde un archivo CSV
    filepath = 'transactions.csv'
    transactions = load_transactions(filepath)

    print("Transacciones cargadas:")
    for i, t in enumerate(transactions):
        print(f"{i + 1}: {t}")

    # Paso 2: Generar itemsets frecuentes
    min_support = 4
    frequent_itemsets = generate_itemsets(transactions, min_support)
    
    # Paso 3: Obtener itemsets cerrados y máximos
    closed_itemsets = get_closed_itemsets(frequent_itemsets)
    maximal_itemsets = get_maximal_itemsets(frequent_itemsets)

    # Paso 4: Mostramos los resultados
    print("\nFrequent Itemsets:")
    for itemset, count in sorted(frequent_itemsets.items(), key=lambda x: (-len(x[0]), -x[1])):
        print(f"{itemset}: {count}")

    print("\nClosed Itemsets:")
    for itemset in closed_itemsets:
        print(itemset)

    print("\nMaximal Itemsets:")
    for itemset in maximal_itemsets:
        print(itemset)
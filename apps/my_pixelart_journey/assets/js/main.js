// Note: pokemonData is now a global variable from pokemon_data.js

document.addEventListener('DOMContentLoaded', () => {
    const pokedexGrid = document.querySelector('.pokedex-grid');

    /**
     * Format Pokemon ID to 3 digits
     * @param {number|string} id 
     * @returns {string}
     */
    function formatPokemonId(id) {
        return id.toString().padStart(3, '0');
    }

    /**
     * Calculate stat percentage (max 150)
     * @param {number} value 
     * @returns {number}
     */
    function getStatPercentage(value, max = 150) {
        const percentage = (value / max) * 100;
        return Math.min(100, Math.max(0, percentage));
    }

    /**
     * Create a Pokemon card element
     * @param {Object} pokemon 
     * @returns {string}
     */
    function createPokemonCard(pokemon, index) {
        const statsHtml = pokemon.stats ? Object.entries(pokemon.stats).map(([label, value]) => `
            <div class="stat-row">
                <span class="stat-label">${label.toUpperCase()}</span>
                <div class="stat-bar-bg">
                    <div class="stat-bar-fill" style="width: ${getStatPercentage(value)}%;"></div>
                </div>
            </div>
        `).join('') : '';

        const typePills = pokemon.types ? pokemon.types.map(type => `
            <span class="type-pill">${type}</span>
        `).join('') : '';

        return `
            <div class="pokemon-card" style="--accent-color: ${pokemon.color}; animation-delay: ${0.1 * (index + 1)}s;">
                <div class="pokemon-id">#${formatPokemonId(pokemon.id)}</div>
                
                <div class="pokemon-image-container">
                    <img src="assets/images/${pokemon.image}" alt="${pokemon.name}" loading="lazy">
                </div>

                <div class="pokemon-info">
                    <h2 class="pokemon-name">${pokemon.name}</h2>
                    ${typePills ? `<div class="types-container">${typePills}</div>` : ''}
                    ${statsHtml ? `<div class="stats-container">${statsHtml}</div>` : ''}
                </div>
            </div>
        `;
    }

    // Render all pokemon
    if (typeof pokemonData !== 'undefined' && Array.isArray(pokemonData) && pokemonData.length > 0) {
        const pokemonCards = pokemonData.map((pokemon, index) => createPokemonCard(pokemon, index)).join('');
        pokedexGrid.innerHTML = pokemonCards;
    } else {
        console.error("Pokemon data is empty or invalid:", typeof pokemonData !== 'undefined' ? pokemonData : 'undefined');
        pokedexGrid.innerHTML = '<p>No Pokemon found.</p>';
    }
});

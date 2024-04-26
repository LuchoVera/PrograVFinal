let currentSlide = 0;
const slides = document.querySelectorAll('.slide');
const arrows = document.querySelectorAll('.arrow');

function showSlide(index) {
    slides.forEach(slide => {
        slide.classList.remove('active');
    });

    slides[index].classList.add('active');
}

function nextSlide() {
    currentSlide++;
    if (currentSlide >= slides.length) {
        currentSlide = 0; // Volver al principio al llegar al final
    }
    showSlide(currentSlide);
    arrows.forEach(arrow => {
        arrow.classList.remove('active');
    });
    arrows[1].classList.add('active');
}

function prevSlide() {
    currentSlide--;
    if (currentSlide < 0) {
        currentSlide = slides.length - 1; // Ir al final al retroceder desde el principio
    }
    showSlide(currentSlide);
    arrows.forEach(arrow => {
        arrow.classList.remove('active');
    });
    arrows[0].classList.add('active');
}

showSlide(currentSlide);
arrows[0].classList.add('active');
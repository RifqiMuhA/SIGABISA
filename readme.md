# SIGABISA: Sistem Informasi Geospasial Ancaman Bencana berbasis Indikator Sosial

## [Link Deploy](https://rifqimuha.shinyapps.io/SIGABISA/)

[![Language: R](https://img.shields.io/badge/language-R-blue.svg)](https://github.com/rifqimuha/SIGABISA)
[![Style: CSS](https://img.shields.io/badge/style-CSS-informational.svg)](https://github.com/rifqimuha/SIGABISA)

## Deskripsi
**SIGABISA** adalah platform analisis kerentanan sosial terhadap bencana di Indonesia yang menggunakan data **SUSENAS 2017** dari BPS untuk 511 kabupaten/kota. Aplikasi ini menyediakan fitur analisis statistik **ANOVA** dan **clustering** (K-Means vs FGWC) guna mendukung pengambilan keputusan yang berbasis data dalam upaya mitigasi bencana.

## Fitur Utama
- Analisis **ANOVA** untuk mengidentifikasi disparitas partisipasi dalam pelatihan kesiapsiagaan bencana  
- **Clustering** menggunakan metode **K-Means** dan **FGWC** untuk menyusun tipologi kerentanan sosial  
- **Visualisasi interaktif** melalui peta dan grafik statistik  
- Dashboard komprehensif yang dilengkapi dengan interpretasi hasil  
- Fitur **export** hasil analisis dalam berbagai format

## Dataset
- **Sumber**: SUSENAS 2017, BPS - Statistics Indonesia  
- **Cakupan**: 511 kabupaten/kota di seluruh Indonesia  
- **Jumlah Variabel**: 17 indikator kerentanan sosial  
- **Fokus Analisis**:
  - `NOTRAINING`: Belum pernah mengikuti pelatihan bencana  
  - `DPRONE`: Tingkat kerentanan terhadap bencana  
  - `POVERTY`: Tingkat kemiskinan  
  - `NOELECTRIC`: Tidak memiliki akses listrik  
  - `ELDERLY`: Persentase lansia
  
## Struktur Analisis

### 1. Analisis ANOVA
- **One-Way ANOVA**: Mengukur disparitas regional partisipasi pelatihan bencana  
- **Two-Way ANOVA**: Menganalisis pengaruh interaksi antara wilayah dan tingkat pendidikan  
- **Post-hoc Analysis**: Mengidentifikasi perbedaan spesifik antar kelompok yang signifikan

### 2. Clustering Analysis
- **K-Means**: Clustering berbasis kemiripan karakteristik sosial ekonomi  
- **FGWC**: Clustering berbasis spasial dengan mempertimbangkan kedekatan geografis  
- **Perbandingan K-Means vs FGWC**: Menunjukkan trade-off antara akurasi statistik dan kemudahan implementasi kebijakan wilayah

---

## Hasil Utama
- **97–99%** masyarakat Indonesia belum pernah mengikuti pelatihan bencana  
- Tidak ditemukan **disparitas regional yang signifikan** dalam partisipasi pelatihan bencana  
- **FGWC** menunjukkan hasil clustering yang lebih **representatif secara geografis** untuk kebijakan daerah  
- Dihasilkan **tipologi kerentanan sosial** sebagai dasar strategi intervensi yang lebih tepat sasaran

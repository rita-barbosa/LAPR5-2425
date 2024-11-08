import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Session } from '../../domain/session';
import { CursoDetailComponent } from '../curso-detail/curso-detail.component';


@Component({
  selector: 'app-curso-angular',
  standalone: true,
  imports: [CommonModule, CursoDetailComponent],
  templateUrl: './curso-angular.component.html',
  styleUrl: './curso-angular.component.css'
})
export class CursoAngularComponent implements OnInit {
selectSession(_t7: any) {
throw new Error('Method not implemented.');
}
  selectedSession?: Session

  sessions: Session[] = [
    { number: 1, topic: 'Introdução' },
    { number: 2, topic: 'Conceitos Básicos' },
  ];

  onSelect(session: Session): void {
    this.selectedSession= session;
  }

  constructor() { 

  }

  ngOnInit(): void {

  }
}
